module Vars (
    Var(..)
    , Env(..)
    , Namespace(..)
) where

import System.Environment

data Env = Env [Namespace] deriving (Show, Eq)
data Namespace = Namespace [Var] deriving (Show, Eq)
data Var = Var String String deriving Eq -- a Var has a key and a value

instance Show Var where
    show (Var k v) = k ++ "=" ++ v
