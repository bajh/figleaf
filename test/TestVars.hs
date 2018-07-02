module TestVars(
    tests
) where

import Test.HUnit
import Vars(Var(..))
import Parse(
    VarSpec(..)
    , matches
    , ParseContext(..)
    , parseArgs)
import Data.Set (Set, fromList)

testVarMatch1 = TestCase (
    assertEqual "for PRD.api.host"
    True
    (matches "PRD" "api" "host" (VarSpec $ fromList [("PRD", "api", "host")])))

testVarNonMatch1 = TestCase(
    assertEqual "for PRD.api.api_key"
    False
    (matches "PRD" "api" "host" (VarSpec $ fromList [("PRD", "api", "api_key")])))

testVarNonMatch2 = TestCase(
    assertEqual "for PRD.oauth.host"
    False
    (matches "PRD" "api" "host" (VarSpec $ fromList [("PRD", "oauth", "host")])))
 
testVarNonMatch3 = TestCase(
    assertEqual "for DEV.api.host"
    False
    (matches "PRD" "api" "host" (VarSpec $ fromList [("STG", "api", "host")])))

testNamespaceMatch1 = TestCase(
    assertEqual "for PRD.api"
    True
    (matches "PRD" "api" "host" (EnvNamespaceSpec "PRD" "api")))

testNamespaceNonMatch1 = TestCase(
    assertEqual "for DEV.api"
    False
    (matches "PRD" "api" "host" (EnvNamespaceSpec "DEV" "api")))

testNamespaceNonMatch2 = TestCase(
    assertEqual "for PRD.oauth"
    False
    (matches "PRD" "api" "host" (EnvNamespaceSpec "PRD" "oauth")))

testEnvMatch1 = TestCase(
    assertEqual "for PRD"
    True
    (matches "PRD" "api" "host" (EnvSpec "PRD")))

testEnvNonMatch1 = TestCase(
    assertEqual "for DEV"
    False
    (matches "PRD" "api" "host" (EnvSpec "DEV")))

testParseArgs1 = TestCase(
    assertEqual "for PRD"
    (Right (EnvSpec "PRD"))
    (parseArgs ["PRD"]))

testParseArgs2 = TestCase(
    assertEqual "for [PRD DEV]"
    (Left "only one env can be specified at a time")
    (parseArgs ["PRD", "DEV"]))

testParseArgs3 = TestCase(
    assertEqual "for PRD.api"
    (Right (EnvNamespaceSpec "PRD" "api"))
    (parseArgs ["PRD.api"]))

testParseArgs4 = TestCase(
    assertEqual "for PRD.api DEV.api"
    (Left "only one env+namespace can be specified at a time")
    (parseArgs ["PRD.api", "DEV.api"]))

testParseArgs5 = TestCase(
    assertEqual "for PRD.api.host"
    (Right (VarSpec (fromList [("PRD", "api", "host")])))
    (parseArgs ["PRD.api.host"]))

testParseArgs6 = TestCase(
    assertEqual "for PRD.api.host DEV.oauth.token"
    (Right (VarSpec (fromList [("PRD", "api", "host"), ("DEV", "oauth", "token")])))
    (parseArgs ["PRD.api.host", "DEV.oauth.token"]))

tests = TestList [ TestLabel "matching var" testVarMatch1
    , TestLabel "matching var when found var does not match" testVarNonMatch1
    , TestLabel "matching var when found namespace does not match" testVarNonMatch2
    , TestLabel "matching var when found env does not match" testVarNonMatch3
    , TestLabel "matching env-namespace" testNamespaceMatch1
    , TestLabel "matching namespace when found env does not match" testNamespaceMatch1
    , TestLabel "matching env when found namespace does not match" testNamespaceNonMatch2
    , TestLabel "matching env" testEnvMatch1
    , TestLabel "non-matching env" testEnvNonMatch1
    , TestLabel "one ENV arg" testParseArgs1
    , TestLabel "two ENVs arg - invalid" testParseArgs2
    , TestLabel "one ENV.namespace arg" testParseArgs3
    , TestLabel "two ENV.namespace args" testParseArgs4
    , TestLabel "one var arg" testParseArgs5
    , TestLabel "two var args" testParseArgs6 ]
