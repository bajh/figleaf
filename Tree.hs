module Tree (
    Tree(..)
    , Assignment(..)
    , tsUpdate
) where

data Tree k v = Tree k [Tree k v] | Leaf k v deriving (Show, Eq)

data Assignment k v = Assignment [k] v deriving (Show, Eq)

-- tUpdate applies key=value assignments to a tree, creating a new "modified" tree
tUpdate :: (Show k, Eq k, Show v, Eq v) => Assignment k v -> Tree k v  -> Tree k v
tUpdate a (Leaf _ _) = aToT a
tUpdate a@(Assignment (k:[]) v) _ = aToT a
tUpdate (Assignment (k:ks) v) (Tree k' ts) = Tree k' (tsUpdate ts (Assignment ks v))

-- tsUpdate searches a list of Trees for the Tree that matches the provided Assignment
-- and updates that Tree as specified by the Assignment
tsUpdate :: (Show k, Eq k, Show v, Eq v) => [Tree k v] -> Assignment k v -> [Tree k v]
tsUpdate ts a@(Assignment (ak:aks) v) =
    case ts of
        [] -> [aToT a]
        (t@(Leaf tk _)):ts' -> if tk == ak then
                (aToT a):ts'
            else t:(tsUpdate ts' a)
        (t@(Tree tk _)):ts' -> if tk == ak then
                (tUpdate a t):ts'
            else 
                t:(tsUpdate ts' a)

-- aToT converts an assignment to a Tree, creating a new Tree node for each segment in the path, except the last, 
-- followed by a final Leaf node representing the last path segment and the value
aToT :: Assignment k v -> Tree k v
aToT (Assignment [k] v) = Leaf k v
aToT (Assignment (k:ks) v) = Tree k [aToT $ Assignment ks v]