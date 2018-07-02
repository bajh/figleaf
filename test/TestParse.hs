module TestParse (
    tests
) where

import Test.HUnit
import Parse
import Vars(Var(..))

testValidVar1 = TestCase (
    assertEqual "for DB_PASSWORD=foo"
        (Right $ Var "DB_PASSWORD" "foo")
        (parseVar "DB_PASSWORD=foo"))
testInvalidVar1 = TestCase (
    assertEqual "for DB_PASSWORDfoo"
    (Left "invalid formatting, expected var got: DB_PASSWORDfoo")
    (parseVar "DB_PASSWORDfoo"))

testValidEnv1 = TestCase (assertEqual "for PRD" (Right $ EnvContext "PRD") (parseEnv "PRD"))
testInvalidEnv1 = TestCase (
    assertEqual "for '\tPRD'"
        (Left "invalid formatting, expected env definition but found indentation")
        (parseEnv "\tPRD"))

testValidNamespace1 = TestCase (
    assertEqual "for \tGQL"
        (Right $ EnvNamespaceContext "PRD" "GQL")
        (parseNamespace (EnvContext "PRD") "\tGQL"))
testInvalidNamespace1 = TestCase (
    assertEqual "for \t\tGQL"
        (Left "invalid formatting, expected namespace definition but found multiple indentations")
        (parseNamespace (EnvContext "PRD") "\t\tGQL"))
testInvalidNamespace2 = TestCase (
    assertEqual "for GQL"
        (Left "invalid formatting, expected namespace definition but did not find opening indentation")
        (parseNamespace (EnvContext "PRD") "GQL"))

-- parsing in Empty Context
testParseEmpty = TestCase(
    assertEqual "for empty context and valid env"
        (Right (EnvContext "PRD", Nothing))
        (parse "PRD" EmptyContext))
testParseEmptyBadInput = TestCase(
    assertEqual "for empty context and invalid env block start"
    (Left "invalid formatting, expected env definition but found indentation")
    (parse "\tPRD" EmptyContext))

-- in EnvContext
testParseWithEnv = TestCase(
    assertEqual "for env context and valid namespace block start"
    (Right (EnvNamespaceContext "PRD" "GQL", Nothing))
    (parse "\tGQL" (EnvContext "PRD")))
testParseWithEnvBadInput1 = TestCase(
    assertEqual "for env context and var block start"
    (Left "invalid formatting, expected namespace definition but found multiple indentations")
    (parse "\t\tDB_PASSWORD=foo" (EnvContext "PRD")))
testParseWithEnvBadInput2 = TestCase(
    assertEqual "for env context and env block start"
    (Left "invalid formatting, expected namespace definition but did not find opening indentation")
    (parse "DEV" (EnvContext "PRD")))

-- in EnvNamespaceContext
testParseWithEnvNamespace = TestCase(
    assertEqual "for env and namespace context and valid env"
    (Right (EnvNamespaceContext "PRD" "GQL", Just $ Var "DB_PASSWORD" "foo"))
    (parse "\t\tDB_PASSWORD=foo" (EnvNamespaceContext "PRD" "GQL")))
testParseNewEnv = TestCase(
    assertEqual "end of env/namespace context with new env block"
    (Right (EnvContext "DEV", Nothing))
    (parse "DEV" (EnvNamespaceContext "PRD" "GQL")))
testParseNewEnvNamespace = TestCase(
    assertEqual "start of new namespace context within env"
    (Right (EnvNamespaceContext "PRD" "USER_INFO", Nothing))
    (parse "\tUSER_INFO" (EnvNamespaceContext "PRD" "GQL")))

tests = TestList [ TestLabel "parsing valid var" testValidVar1
    , TestLabel "parsing var with no key/val separator" testInvalidVar1
    , TestLabel "parsing valid env block start" testValidEnv1
    , TestLabel "parsing env block start with invalid \t" testInvalidEnv1
    , TestLabel "parsing valid namespace block" testValidNamespace1
    , TestLabel "parsing invalid namespace block -- too many indentations" testInvalidNamespace1
    , TestLabel "parsing invalid namespace block -- unexpected block end" testInvalidNamespace2
    , TestLabel "parsing valid env block start" testParseEmpty
    , TestLabel "parsing invalid env block start" testParseEmptyBadInput
    , TestLabel "parsing valid namespace block start" testParseWithEnv
    , TestLabel "parsing invalid var block start in env context" testParseWithEnvBadInput1
    , TestLabel "parsing invalid env block start in env context" testParseWithEnvBadInput2
    , TestLabel "parsing valid var in env namespace context" testParseWithEnvNamespace
    , TestLabel "parsing end of var block in env namespace context" testParseNewEnv
    , TestLabel "parsing start of new namespace in env context" testParseNewEnvNamespace ]