module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestParseAndSerialize
import TestCommands
import TestUpdates

allTests = concat $ fmap hUnitTestToTests [TestParseAndSerialize.tests, TestCommands.tests, TestUpdates.tests]
main = defaultMain allTests