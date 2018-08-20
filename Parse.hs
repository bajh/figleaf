module Parse (
    runParse
) where

import Data.List.Split
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity

import Tree (Tree(..)) 

-- TODOS:
-- * escaping for newlines and leading tabs
-- * Error handling for invalid changes in depth in the config input

data ParseState = ParseState {
    depth :: Int
    , input :: [String]
    }

type Parse a = StateT ParseState (Except String) a

parseDepth :: String -> (Int, String)
parseDepth s = let (tabs, rest) = span (== '\t') s in (length tabs, rest)

parseTree :: Parse (Tree String String)
parseTree = do
    st@(ParseState depth (l:ls)) <- get
    let (depth', val) = parseDepth l
    put st { input = ls }
    st <- get
    case span (/= '=') val of
        (k, "") -> do
            put st { depth = depth + 1 }
            Tree k <$> parse
        (k, '=':v) -> return $ Leaf k v

parse :: Parse [Tree String String]
parse = do
    -- figure out what the current depth is to see if we're still at the same level
    st@(ParseState depth input) <- get
    case input of
        [] -> return []
        l:ls -> let (depth', val) = parseDepth l in
            if depth == depth' then do
                -- parseTree will keep parsing until it reaches another line with the same depth as itself or higher
                t <- parseTree
                ts <- parse
                return $ t:ts
            else do
                -- We're going up, thus concluding this level
                    put st { depth = depth' }
                    return []

runParse s = case (runExceptT . evalStateT parse . ParseState 0 . splitOn "\n") s of
    (Identity (Left e)) -> Left e
    (Identity (Right ts)) -> Right $ Tree "default" ts