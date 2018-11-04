module Serialize(
    serialize
) where

import Tree (Tree(..)) 

import Data.List

-- replicateTs returns a string of tab characters repeated the specified number of times
replicateTs = flip replicate '\t'

-- serializeNs serializes a list of trees at a specific depth
serializeNs d = intercalate "\n" . map serializeSiblings
    where serializeSiblings = serializeN d
-- serializeN serializes a tree at a specific depth
serializeN d (Leaf k v) = replicateTs d ++ k ++ "=" ++ v
serializeN d (Tree "default" ts) = serializeNs 0 ts
serializeN d (Tree k []) = replicateTs d ++ k
serializeN d (Tree k ts) = replicateTs d ++ k ++ "\n" ++ serializeNs (d + 1) ts

-- serialize converts a Tree into a string representation for disk storage or network transmission
serialize :: [Tree String String] -> String
serialize = serializeNs 0