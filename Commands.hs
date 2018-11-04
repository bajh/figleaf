module Commands (
    tFilter
    , tsFilter
    , tApply
    , fParse
    , parseAssignments
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
fParse = splitOn "."

-- parseAssignments takes a list of command line args representing modifications to the environment tree
-- and converts each into an Assignment
-- Ugh this logic of folding an Either, stopping at the first returned Left, is definitely already
-- a thing (the opposite of sconcat?) but I can't seem to figure out what it is right now!
parseAssignments :: [String] -> Either String [Assignment String String]
parseAssignments = foldr (\a acc ->
    case acc of
        Left e -> Left e
        Right args -> case parseAssignment a of
            Left e -> Left e
            Right a -> Right (a:args)) (Right [])

parseAssignment :: String -> Either String (Assignment String String)
parseAssignment s = case splitOn "=" s of
    [keypath, v] -> Right $ Assignment (splitOn "." keypath) v
    _ -> Left $ "invalid assignment spec: " ++ s

-- tFilter applies a PathFilter to a Tree, returning the sub-tree selected by the filter (that is, the sub-tree
-- located at the path defined by the filter)
tsFilter :: [Tree String String] -> PathFilter -> Either String (Tree String String)
tsFilter ts fs =
    sconcat $ Data.List.NonEmpty.map (`tFilter` fs) nonEmptyTs
    where nonEmptyTs = case nonEmpty ts of Just ts -> ts

tFilter :: Tree String String -> PathFilter -> Either String (Tree String String)
tFilter t@(Leaf k _) [n] = if n == k then Right t else  Left $ "no " ++ n
tFilter t@(Tree k _) [n] = if n == k then Right t else Left $ "no " ++ n
tFilter t@(Leaf _ _) (f:fs) = Left $ "no " ++ f
tFilter t@(Tree k ts) f@(n:ns) =
    if n == k then
        tsFilter ts ns
    else
        Left $ "no " ++ n