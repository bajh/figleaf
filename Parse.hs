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

-- ParseState encapsulates the current depth in a tree that is being parsed as well
-- as lines of input that still need to be parsed. It is meant to be used in a State
-- monad
data ParseState = ParseState {
    depth :: Int
    , input :: [String]
    }

-- Parse combines a ParseState with exception handling signalling an issue
-- parsing input into a Tree
type Parse a = StateT ParseState (Except String) a

-- parseDepth determines what level of depth in a tree the current line represents
parseDepth :: String -> (Int, String)
parseDepth s = let (tabs, rest) = span (== '\t') s in (length tabs, rest)

-- parseTree returns a Parse monad that when evaluated with an input string will
-- wrap the next fully parsed tree in the input string and have its state updated
-- to reflect the current depth level and remaining input
parseTree :: Parse (Tree String String)
parseTree = do
    st@(ParseState depth (l:ls)) <- get
    let (depth', val) = parseDepth l
    put st { input = ls }
    st <- get
    case span (/= '=') val of
        -- if this line does not include a =, it represents a new namespace and we
        -- need to create a new tree at the next level of depth
        (k, "") -> do
            put st { depth = depth + 1 }
            Tree k <$> parse
        -- otherwise, this is a key-value pair that should be added to the nodes of this tree
        (k, '=':v) -> return $ Leaf k v

-- parse returns a Parse monad that when evaluated will parse the input until this level of the
-- tree ends, providing a list of sibling trees
parse :: Parse [Tree String String]
parse = do
    -- figure out what the current depth is to see if we're still at the same level
    st@(ParseState depth input) <- get
    case input of
        -- EOF
        [] -> return []
        l:ls -> let (depth', val) = parseDepth l in
            if depth == depth' then do
                -- parseTree will keep parsing until it reaches another line with the same depth as itself or higher
                t <- parseTree
                ts <- parse
                return $ t:ts
            else do
                -- We're going up, thus concluding this stream of sibling nodes
                put st { depth = depth - 1 }
                return []

runParse s = case (runExceptT . evalStateT parse . ParseState 0 . splitOn "\n") s of
    (Identity (Left e)) -> Left e
    (Identity (Right ts)) -> Right ts