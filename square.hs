import qualified Data.Map.Strict as M
import Data.Char
import Debug.Trace

type Aliases = M.Map String Value
type Scope = [Aliases]
data VM = VM { scope :: Scope }

data Value = Null | Literal String | Procedure Statement | IProcedure Statement | Lookup String deriving (Eq)
data Statement = Block [Statement] | Statement String [Value] deriving (Eq)

instance Show Value where
    show Null = "Null"
    show (Literal string) = "Literal " ++ (show string)
    show (Procedure statement) = "Procedure<>"
    
instance Show VM where
    show vm = show (scope vm)

-- Get the actual value of a statement
valueof :: VM -> Value -> Value
valueof _ Null = Null
valueof _ (Literal string) = (Literal string)
valueof _ (Procedure statement) = (Procedure statement)
valueof vm (IProcedure statement) = resvalue
    where (resvm, resvalue) = exec vm (IProcedure statement)
valueof vm (Lookup name) = vmget vm name

valuestr :: VM -> Value -> String
valuestr _ Null = "Null"
valuestr _ (Literal str) = str 
valuestr _ (Procedure _) = "Procedure" -- TODO, maybe make it the actual Procedure?
valuestr vm (IProcedure statement) = valuestr vm $ valueof vm (IProcedure statement)
valuestr vm (Lookup name) = valuestr vm $ valueof vm (Lookup name)
    
-- Get/set aliases, based on the provided scope
get :: Scope -> String -> Value
get (aliases:next) what = trylookup (M.lookup what aliases)
    where
        trylookup (Just value) = value
        trylookup Nothing = get next what
get [] _ = Null

-- Create a new local variable
setl :: Scope -> String -> Value -> Scope
setl (aliases:next) what value = (M.insert what value aliases) : next
setl [] what value = [M.insert what value M.empty]

-- Set an existing variable (in any scope, this or upwards)
set :: Scope -> String -> Value -> Scope
set [] what value = setl [] what value
set scope what value = setr scope
    where
        setr (aliases:next)
            | M.member what aliases = (M.insert what value aliases) : next
            | otherwise = aliases : (setr next)

-- Shortcut functions to get/set aliases
vmget :: VM -> String -> Value
vmget vm what = get (scope vm) what

vmsetl :: VM -> String -> Value -> VM
vmsetl vm what value = VM { scope = setl (scope vm) what value }

vmset :: VM -> String -> Value -> VM
vmset vm what value = VM { scope = set (scope vm) what value }

vmenter :: VM -> VM
vmenter vm = VM { scope = (M.empty : (scope vm)) }

vmleave :: VM -> VM
vmleave vm = VM { scope = tail (scope vm) }


exec :: VM -> Value -> (VM, Value)
-- Execute a block (list of statements)
exec vm (Procedure (Block statements)) = trace "BLOCK" $ execr statements (vmenter vm, Null)
    where
        execr :: [Statement] -> (VM, Value) -> (VM, Value)
        execr (statement:more) (vm, value) = execr more (exec vm (Procedure statement))
        execr [] (vm, value) = (vm, value)
        
-- Execute a single statement
exec vm (Procedure (Statement fun args)) = trace "STATEMENT" $ dofun fun literalargs
    where
        dofun :: String -> [Value] -> (VM, Value)
        -- Built-in functions
        dofun "trace" args = trace (valuestr vm $ head args) $ (vm, Null)
        dofun "concat" args = (Literal (unwords $ map (valuestr vm) args), vm)
        dofun "show" args = trace (show $ head args) $ (vm, Null)
        dofun "var" ((Literal name):value:_) = (vmsetl vm name value, Null)
        dofun "set" ((Literal name):value:_) = (vmset vm name value, Null)
        dofun "get" ((Literal name):_) = (vm, vmget vm name)
        -- User-defined functions
        dofun fun args
            | userfun /= Null = exec funvm userfun
            | otherwise = trace ("Error: function " ++ fun ++ " does not exist!") $ (vm, Null)
        
        userfun = (vmget funvm fun)
        funvm = funvmr vm args 1
        funvmr :: VM -> [Value] -> Integer -> VM
        funvmr vm (arg:args) num = funvmr (vmsetl vm ("arg" ++ (show num)) arg) args (num + 1)
        funvmr vm _ num = vmsetl vm "numargs" $ Literal (show (num-1))
        
        literalargs = foldl valueof vm args

-- I can't think of a situation where these things would actually be executed, but meh...
-- Immidiate Procedure & variable lookup
exec vm (IProcedure proc) = exec vm (Procedure proc)
exec vm (Lookup name) = (vm, vmget vm name)

-- Nulls and Literals just evaluate to themselves
exec vm Null = trace "ERROR: NULL NOT EXECUTABLE" $ (vm, Null)
exec vm (Literal value) = trace "LITERAL" $ (vm, Literal value)

testVM = VM { scope = [] }
testCode = Procedure (Block [
            Statement "trace" [Literal "hello!"]
            ])



