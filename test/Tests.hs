module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestParseAndSerialize
import TestCommands
import TestUpdates

allTests = hUnitTestToTests TestParseAndSerialize.tests ++ hUnitTestToTests TestCommands.tests ++ hUnitTestToTests TestUpdates.tests
main = defaultMain allTests