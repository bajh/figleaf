module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestParse
import TestVars

allTests = (hUnitTestToTests TestParse.tests) ++ (hUnitTestToTests TestVars.tests)
main = defaultMain allTests