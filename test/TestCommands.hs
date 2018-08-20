module TestCommands (
    tests
) where

import Test.HUnit
import Commands(tFilter)
import Tree(Tree(..))

testFilter1 = TestCase (
    assertEqual "for top-level match"
    (Right $ Tree "PRD" [Leaf "DB_PASSWORD" "foo", Leaf "API_KEY" "bar"])
    (tFilter (Tree "PRD" [Leaf "DB_PASSWORD" "foo", Leaf "API_KEY" "bar"]) ["PRD"]))

testFilter2 = TestCase (
    assertEqual "nested leaf match"
    (Right $ Leaf "DB_PASSWORD" "foo")
    (tFilter (Tree "PRD" [Leaf "DB_PASSWORD" "foo"]) ["PRD", "DB_PASSWORD"]))

testFilter3 = TestCase (
    assertEqual "nested tree match"
    (Right $ Tree "a4ef5" [Leaf "DB_PASSWORD" "foo", Leaf "API_KEY" "boo"])
    (tFilter (Tree "SANDBOX" [Tree "a4ef5" [Leaf "DB_PASSWORD" "foo", Leaf "API_KEY" "boo"], Tree "c3e35" [Leaf "DB_PASSWORD" "bar"]] ) ["SANDBOX", "a4ef5"]))

testFilter4 = TestCase (
    assertEqual "non-match"
    (Left "no DB_PASSWORD")
    (tFilter (Tree "PRD" [Leaf "DB_USER" "foo"]) ["PRD", "DB_PASSWORD"]))

tests = TestList [ TestLabel "match 1" testFilter1
    , TestLabel "match 2" testFilter2
    , TestLabel "match 3" testFilter3
    , TestLabel "match 4" testFilter4 ]