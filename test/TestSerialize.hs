module TestSerialize (
    tests
) where

import Test.HUnit
import Serialize(serialize)
import Tree(Tree(..))

tests = TestList [TestLabel "parsing top-level leaves" testLeaves
    , TestLabel "parsing top-level branches" testEmptyBranches
    , TestLabel "mixed leaves and empty branches" testMixedLeavesEmptyBranches
    , TestLabel "leaves nested beneath one branch" testNestedLeaves
    , TestLabel "deeply nested branches" testNestedBranches
    , TestLabel "mix of branches and leaves" testMix1]