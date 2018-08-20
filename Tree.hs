module Tree (
    Tree(..)
    , Assignment(..)
    , tUpdate
) where

data Tree k v = Tree k [Tree k v] | Leaf k v deriving (Show, Eq)

data Assignment k v = Assignment [k] v deriving (Show, Eq)

-- tUpdate applies key=value assignments to a tree, creating a new "modified" tree
tUpdate :: (Eq k, Eq v) => Assignment k v -> Tree k v  -> Tree k v
tUpdate a@(Assignment (k':ks) v') (Tree k ts) = Tree k (tsUpdate ts a)

-- tsUpdate searches a list of Trees for the Tree that matches the provided Assignment
-- and updates that Tree as specifieid by the Assignment
tsUpdate :: (Eq k, Eq v) => [Tree k v] -> Assignment k v -> [Tree k v]
tsUpdate [] a = [aToT a]
tsUpdate (t:ts) a@(Assignment [k'] _) =
    case t of
        (Tree k (_:ts)) -> if k == k' then aToT a:ts else t:tsUpdate ts a
        l@(Leaf k _) -> if k == k' then aToT a:ts else t:tsUpdate ts a
tsUpdate (t:ts) a@(Assignment (k':ks) v) =
    case t of
        (Tree k _) -> if k == k' then tUpdate (Assignment ks v) t:ts else t:tsUpdate ts a
        -- For now, just replace the leaf. Technically, though, it's possible we could have a Leaf that
        -- is being replaced with a Tree, in which case I guess the best thing to do is create the new
        -- node then go back and add the original Leaf to it if necessary so the original Leaf's information
        -- is not lost
        l@(Leaf k v) -> if k == k' then aToT a:ts else l:tsUpdate ts a

-- aToT converts an assignment to a Tree, creating a new Tree node for each segment in the path, except the last, 
-- followed by a final Leaf node representing the last path segment and the value
aToT :: Assignment k v -> Tree k v
aToT (Assignment [k] v) = Leaf k v
aToT (Assignment (k:ks) v) = Tree k [aToT $ Assignment ks v]