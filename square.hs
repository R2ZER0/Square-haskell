import qualified Data.Map.Strict as M
import Data.Char
import Debug.Trace

type Aliases = M.Map String Value
type Scope = [Aliases]
data VM = VM { scope :: Scope }

instance Show VM where
    show vm = show (scope vm)

data Value = Value String | Expression [Value] | Procedure [Value] | ValueList [Value]