module Main where

import Vars(Var(..))
import Parse(
    toVars
    , parseArgs
    , ParseContext(EmptyContext, EnvNamespaceContext)
    , matches, VarSpec(..))
import System.IO
import System.Environment

getVars :: Handle -> ParseContext -> IO [(ParseContext, Var)]
getVars h ctx = do
    vars <- toVars h ctx
    case vars of
        (Left "EOF") -> return []
        (Right (ctx2, v)) -> do
            vars <- getVars h ctx2
            return ((ctx2, v):vars)

printIfMatchesSpec :: [(ParseContext, Var)] -> VarSpec -> IO ()
printIfMatchesSpec [] spec = return ()
printIfMatchesSpec (((EnvNamespaceContext e n), Var k v):vars) spec = do
    if matches e n k spec then
        putStrLn (show (Var k v))
    else
        return ()
    printIfMatchesSpec vars spec

doCommand argv = case argv of
    ("enable":args) -> do
        h <- openFile "./config" ReadMode
        case parseArgs args of
            Left e -> putStrLn e
            Right specs -> do
                vars <- getVars h EmptyContext
                putStrLn (show vars)
                printIfMatchesSpec (vars) specs
    _ -> putStrLn "Usage:"

main = getArgs >>= doCommand
