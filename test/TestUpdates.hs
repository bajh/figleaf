module TestUpdates (
    tests
) where

import Test.HUnit
import Tree(
    Tree(..)
    , tsUpdate
    , Assignment(..))

updateTest d expT inA inT = TestCase (assertEqual d expT (tsUpdate inT inA))

testUpdate1 = updateTest
    "for tree with just leaf node"
    [Leaf "DB_NAME" "users"]
    (Assignment ["DB_NAME"] "users")
    [Leaf "DB_NAME" "people"]

testUpdate2 = updateTest
    "for tree with nested nodes 1"
    [
        Tree "PRD" [Leaf "DB_NAME" "prd_users"]
        , Tree "STG" [Leaf "DB_NAME" "stg_users"]
        , Tree "DEV" [Leaf "DB_NAME" "users"]
    ]
    (Assignment ["DEV", "DB_NAME"] "users")
    [
        Tree "PRD" [Leaf "DB_NAME" "prd_users"]
        , Tree "STG" [Leaf "DB_NAME" "stg_users"]
        , Tree "DEV" [Leaf "DB_NAME" "dev_users"]
    ]

testUpdate3 = updateTest
    "for tree with nested nodes 2"
    [
        Tree "PRD" [Tree "graphql" [Leaf "API_KEY" "789", Leaf "HOST" "localhost"]],
        Tree "DEV" [Tree "graphql" [Leaf "API_KEY" "456"]]
    ]
    (Assignment ["PRD", "graphql", "API_KEY"] "789")
    [
        Tree "PRD" [Tree "graphql" [Leaf "API_KEY" "123", Leaf "HOST" "localhost"]],
        Tree "DEV" [Tree "graphql" [Leaf "API_KEY" "456"]]
    ]

testUpdate4 = updateTest
    "adding new leaf node to tree"
    [Tree "STG" [], Leaf "CERT" "./cert", Tree "PRD" [Leaf "api_key" "123"]]
    (Assignment ["PRD", "api_key"] "123")
    [Tree "STG" [], Leaf "CERT" "./cert", Tree "PRD" []]

tests = TestList [ TestLabel "updating tree 1" testUpdate1
    , TestLabel "updating tree 2" testUpdate2
    , TestLabel "updating tree 3" testUpdate3
    , TestLabel "updating tree 4" testUpdate4 ]