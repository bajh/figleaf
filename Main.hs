{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import Parse(runParse)
import Tree(tsUpdate)
import Commands(fParse, parseAssignments, tApply, tsFilter)
import Serialize(serialize)

usage = "usage: figleaf [apply|set] arg1 [arg2...]"

getConfigPath = do
    home <- getHomeDirectory
    return $ home </> ".fig_config"

createConfigIfNotExists configF = doesFileExist configF >>= \exists ->
    if not exists
        then writeFile configF ""
        else return ()

getCommandArgs = do
    args <- getArgs
    case args of
        a:args -> return (a, args)
        _ -> do
            print usage
            exitWith $ ExitFailure 1

main = do
    (a, args) <- getCommandArgs
    configF <- getConfigPath
    createConfigIfNotExists configF
    -- TODO: should lock this file to prevent concurrent modifications
    s <- readFile configF
    -- parse the config tree from the serialized file contents
    case runParse s of
        Right ts -> case a of
            -- parse all of the filters specified in the args and apply them
            -- to the tree then print out each of the resulting sub-trees
            "apply" -> mapM_ (\case
                    Right t -> tApply t
                    Left e -> putStrLn $ "error: " ++ e)  (map (parseFilterAndApply ts) args)
            -- apply the specified changes to the tree, serialize it to a
            -- string and save it to the config
            "set" -> case parseAssignments args of
                Right as -> writeFile configF $ serialize $ foldr (flip tsUpdate) ts as
                Left e -> putStrLn $ "error: " ++ e
            _ -> putStrLn usage
        Left e -> putStrLn e

-- parseFilterAndApply parses a PathFilter and then applies it to a tree
-- to get the subtree specified by the filter
parseFilterAndApply ts = tsFilter ts . fParse