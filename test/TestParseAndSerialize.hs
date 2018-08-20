module TestParseAndSerialize (
    tests
) where

import Test.HUnit
import Parse(runParse)
import Serialize(serialize)
import Tree(Tree(..))

validCase ts s l = map (TestLabel l) [
    TestCase(assertEqual ("parsing " ++ s) (Right $ Tree "default" ts) (runParse s))
    , TestCase(assertEqual ("serializing " ++ s) s (serialize (Tree "default" ts))) ]

testLeaves = validCase
    [Leaf "NODE_ENV" "PRD", Leaf "PASSWORD" "123", Leaf "PORT" "6000" ]
    "NODE_ENV=PRD\nPASSWORD=123\nPORT=6000"

testEmptyBranches = validCase
    [Tree "PRD" [], Tree "STG" [], Tree "DEV" []]
    "PRD\nSTG\nDEV"

testMixedLeavesEmptyBranches = validCase
    [Tree "PRD" [], Leaf "PASSWORD" "123", Leaf "PORT" "6000"]
    "PRD\nPASSWORD=123\nPORT=6000"

testNestedLeaves = validCase
    [Tree "PRD" [Leaf "NODE_ENV" "PRD", Leaf "PASSWORD" "123",  Leaf "PORT" "6000"]]
    "PRD\n\tNODE_ENV=PRD\n\tPASSWORD=123\n\tPORT=6000"

testNestedBranches = validCase
    [Tree "DEV" [Tree "readonly" [Tree "v1" [Leaf "PASSWORD" "123"]]]]
    "DEV\n\treadonly\n\t\tv1\n\t\t\tPASSWORD=123"

testMix1 = validCase
    [Leaf "API_KEY" "whales", Tree "DEV" [Tree "readonly" [Leaf "DB_PASSWORD" "chickens", Leaf "USER" "chickenman"], Leaf "GOPATH" "$HOME"], Leaf "KAFKA_BROKERS" "broker1,broker2", Tree "PRD" [Leaf "DB_PASSWORD" "gnats"]]
    "API_KEY=whales\nDEV\n\treadonly\n\t\tDB_PASSWORD=chickens\n\t\tUSER=chickenman\n\tGOPATH=$HOME\nKAFKA_BROKERS=broker1,broker2\nPRD\n\tDB_PASSWORD=gnats"

tests = TestList $ concat [ testLeaves "top-level leaves"
    , testEmptyBranches "top-level branches"
    , testMixedLeavesEmptyBranches "mixed leaves and empty branches"
    , testNestedLeaves "leaves nested beneath one branch"
    , testNestedBranches "deeply nested branches"
    , testMix1 "mix of branches and leaves" ]