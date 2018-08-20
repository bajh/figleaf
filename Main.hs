{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import Parse(runParse)
import Tree(tUpdate)
import Commands(fParse, aParse, parseArgs, tApply, tFilter)
import Serialize(serialize)

usage = "usage: figleaf [apply|set] arg1 [arg2...]"
configF = "./fig_config" -- TODO: put this somewhere real, like ~/.config/figleaf/figleaf.cfg

main = do
    a:args <- getArgs
    s <- readFile configF
    -- Parse the config tree from the serialized file contents
    -- TODO: make this work with an empty file
    case runParse s of
        Right t -> case a of
            -- TODO: I feel like there is a more monad-y way to do this, or at least a way that involves
            -- a smaller lambda (worst case, abstract this lambda out into its own function)
            -- TODO: Right now if there are any overlapping variables in the tree they'll be printed
            -- multiple times, which won't cause any problems but is unnecessary. Also, if you try to set any
            -- variables that conflict with each other there should be an error.
            "apply" -> mapM_ (\case
                    Right t -> tApply t
                    Left e -> putStrLn $ "error: " ++ e)  (map (tFilter t . fParse) args)
            "set" -> case parseArgs args of
                -- Apply the specified changes to the tree, serialize it to a string and save it to the config
                Right args -> writeFile configF $ serialize $ foldr tUpdate t args
                Left e -> putStrLn $ "error: " ++ e
            _ -> putStrLn usage
        Left e -> putStrLn e