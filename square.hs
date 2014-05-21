import Data.Map.Strict as M
import Data.Char
import Debug.Trace

type Aliases = Map String Value
type Scope = [Aliases]
data VM = VM { scope :: Scope }

data Value = Null | Literal String | Procedure Statement
data Statement = Block [Statement] | Statement String [Value]

instance Show Value where
    show Null = "<Null>"
    show (Literal string) = string
    show (Procedure statement) = "<Procedure>"

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
set scope what value = setr scope
    where
        setr (aliases:next)
            | M.member what aliases = (M.insert what value aliases) : next
            | otherwise = aliases : (setr next)
set [] what value = setl [] what value

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
exec vm (Procedure (Block statements)) = execr statements (vmenter vm, Null)
    where
        execr :: [Statement] -> (VM, Value) -> (VM, Value)
        execr (statement:more) (vm, value) = execr more (exec vm (Procedure statement))
        execr [] (vm, value) = (vm, value)
        
-- Execute a single statement
exec vm (Procedure (Statement fun args)) = dofun fun
    where
        dofun :: String -> [Value] -> (VM, Value)
        dofun "trace" args = trace (show $ head args) $ (vm, Null)
        dofun "var" ((Literal name):value:_) = (vmsetl vm name value, Null)
        dofun "set" ((Literal name):value:_) = (vmset vm name value, Null)
        dofun "get" ((Literal name):_) = (vm, vmget vm name)
        dofun fun args = exec funvm (get funvm fun)
        
        funvm = funvmr args 1
        funvmr :: VM -> [Value] -> Integer -> VM
        funvmr vm (arg:args) num = funvmr (vmsetl vm ("arg" ++ (show num)) arg) args (num + 1)
        funvmr vm _ num = vmsetl vm "numargs" $ Literal (show num)

-- Attempting to execute something that you shouldn't!
exec vm _ = trace "ERROR: NOT EXECUTABLE" $ (vm, Null)