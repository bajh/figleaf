module Parse (
    ParseContext(..)
    , toVars
    , parse
    , parseVar
    , parseEnv
    , parseNamespace
    , VarSpec(..)
    , matches
    , parseArgs
) where
    
import Vars (Var(..) , Env(..), Namespace(..))

import Data.List.Split
import Data.Set (Set, member, union, fromList)
import System.IO

data ParseContext = EmptyContext
    | EnvContext String
    | EnvNamespaceContext String String deriving (Show, Eq) -- No existing context, An env or an env and namespace

-- It feels like instead of "IO String" and "IO (ParseContext, Var)" this should be something like "Monad String"
-- and "Monad (ParseContext, Var)" and that that should help with testing somehow. But I'm not sure how to do it
toVars :: Handle -> ParseContext -> IO (Either String (ParseContext, Var))
toVars inp ctx = do
    atEOF <- hIsEOF inp
    if atEOF
        then return (Left "EOF")
        else do
            s <- hGetLine inp
            case parse s ctx of
                -- the type we're matching against is an Either String (ParseContext, Maybe Var)
                -- but the type we're returning it an Either String (ParseContext, Var). Even though in this case the Right side doesn't
                -- come into play and we're only matching the Left side we still need to make a new Either variable that is of the correct type
                Left err -> return (Left err) 
                Right (ctx2, Just v) -> return (Right (ctx2, v))
                Right (ctx2, Nothing) -> toVars inp ctx2

parse :: String -> ParseContext -> Either String (ParseContext, Maybe Var)
parse s EmptyContext = case parseEnv s of
    Right ctx -> Right (ctx, Nothing)
    Left e -> Left e

parse s ctx@(EnvContext _) = case (parseNamespace ctx s) of
    Right ctx2 -> Right (ctx2, Nothing)
    Left e -> Left e
 
parse ('\t':'\t':cs) ctx@(EnvNamespaceContext _ _) = case parseVar cs of
    Right v -> Right (ctx, Just v)
    Left e -> Left e
 
parse s@('\t':cs) (EnvNamespaceContext env _) = case parseNamespace (EnvContext env) s of
    Right ctx2  -> Right (ctx2, Nothing)
    Left e -> Left e

parse s _ = case parseEnv s of
    Right ctx -> Right (ctx, Nothing)
    Left e -> Left e

parseEnv :: String -> Either String ParseContext
parseEnv ('\t':_) = Left "invalid formatting, expected env definition but found indentation"
parseEnv s = Right $ EnvContext s

parseNamespace :: ParseContext -> String -> Either String ParseContext
parseNamespace _ ('\t':'\t':_) = Left "invalid formatting, expected namespace definition but found multiple indentations"
parseNamespace (EnvContext env) ('\t':n) = Right $ (EnvNamespaceContext env n)
parseNamespace (EnvContext _) _ = Left "invalid formatting, expected namespace definition but did not find opening indentation"

-- A var must follow the pattern: (key:w+)=(val:\w+). Opening \t\ts will be stripped out
parseVar :: String -> Either String Var
parseVar s = case splitOn "=" s of
    [k, v] -> Right (Var k v)
    _ -> Left ("invalid formatting, expected var got: " ++ s)

-- VarSpecSet can require an env
-- OR a namespace
-- OR a series of vars
data VarSpec = EnvSpec String
    | EnvNamespaceSpec String String
    | VarSpec (Set (String, String, String)) deriving (Show, Eq)

matches :: String -> String -> String -> VarSpec -> Bool
matches currEnv _ _ (EnvSpec wantedEnv) =
    currEnv == wantedEnv

matches currEnv currNamespace _ (EnvNamespaceSpec wantedEnv wantedNamespace) =
    (currEnv == wantedEnv) && (currNamespace == wantedNamespace)

matches currEnv currNamespace k (VarSpec wantedVars) =
    (member (currEnv, currNamespace, k) wantedVars)

-- It feels like there should be a fancy way to take advantage of the monadic properties of this being both
-- a (wrapper of a ) Set and an Either???
parseArgs :: [String] -> Either String VarSpec
parseArgs [] = Left "no operations specified"
parseArgs (a:[]) = case (parseArg a) of
    Left e -> Left e
    -- Is there a better way to do this?
    Right (EnvNamespaceSpec e n) -> Right (EnvNamespaceSpec e n)
    Right (EnvSpec e) -> Right (EnvSpec e)
    Right (VarSpec s) -> Right (VarSpec s)
parseArgs (a:as) = case (parseArg a) of
    Left e -> Left e
    Right (EnvNamespaceSpec _ _) -> Left "only one env+namespace can be specified at a time"
    Right (EnvSpec _) -> Left "only one env can be specified at a time"
    Right (VarSpec nextSpec) -> case (parseArgs as) of
        Left e -> Left e
        Right (VarSpec specs) -> Right (VarSpec (union nextSpec specs))

parseArg :: String -> Either String VarSpec
parseArg a = case splitOn "." a of
    [e, n, v] -> Right (VarSpec (fromList [(e, n, v)]))
    [e, n] -> Right (EnvNamespaceSpec e n)
    [e] -> Right (EnvSpec a)
    _ -> Left "invalid formatting, expected (e)?.(n)?.(v)"