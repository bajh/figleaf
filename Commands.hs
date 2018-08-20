module Commands (
    tFilter
    , tApply
    , fParse
    , parseArgs
) where

import Tree(Tree(..), Assignment(..))
import Data.Semigroup
import Data.List.NonEmpty
import Data.List.Split

-- A PathFilter represents the path that should be taken through a tree to reach a sub-tree that
-- should be applied
type PathFilter = [String]

-- tApply prints all the key values pairs within a tree to stdout
tApply :: Tree String String -> IO ()
tApply (Leaf k v) = putStrLn (k ++ "=" ++ v)
tApply (Tree _ ts) = mapM_ tApply ts

-- fParse converts a string of the format "admin.PRD.DB" to a PathFilter
fParse :: String -> PathFilter
fParse = ("default" :) . splitOn "."

-- parseArgs takes a list of command line args representing modifications to the environment tree
-- and converts each into an Assignment
-- Ugh this logic of folding an Either, stopping at the first returned Left, is definitely already
-- a thing (the opposite of sconcat?) but I can't seem to figure out what it is right now!
parseArgs :: [String] -> Either String [Assignment String String]
parseArgs = foldr (\a acc ->
    case acc of
        Left e -> Left e
        Right args -> case parseArg a of
            Left e -> Left e
            Right a -> Right (a:args)) (Right [])

parseArg :: String -> Either String (Assignment String String)
parseArg s = case splitOn "=" s of
    [keypath, v] -> Right $ Assignment (splitOn "." keypath) v
    _ -> Left $ "invalid assignment spec: " ++ s

-- tFilter applies a PathFilter to a Tree, returning the sub-tree selected by the filter (that is, the sub-tree
-- located at the path defined by the filter)
tFilter :: Tree String String -> PathFilter -> Either String (Tree String String)
tFilter t@(Leaf k _) [n] = if n == k then Right t else  Left $ "no " ++ n
tFilter t@(Tree k _) [n] = if n == k then Right t else Left $ "no " ++ n
tFilter t@(Leaf _ _) (f:fs) = Left $ "no " ++ f
tFilter t@(Tree k ts) f@(n:ns) =
    if n == k then
        -- call filter with the remaining path filters on each tree, return Right if any of them is Right
        sconcat $ Data.List.NonEmpty.map (`tFilter` ns) nonEmptyTs
    else
        -- call filter on the next fs with the same tree to see if it matches
        Left $ "no " ++ n
    where nonEmptyTs = case nonEmpty ts of Just ts -> ts