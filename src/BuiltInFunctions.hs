

module BuiltInFunctions
    ( anyOfLiteral
    , noneOfLiteral
    , maybeLiteral
    ) where

import           ProgramAst

anyOfLiteral :: String -> FuncInvocation
anyOfLiteral string = BuiltInFuncInvocation AnyOf [ArgLiteral string]

noneOfLiteral :: String -> FuncInvocation
noneOfLiteral string = BuiltInFuncInvocation NoneOf [ArgLiteral string]

maybeLiteral :: String -> FuncInvocation
maybeLiteral char = BuiltInFuncInvocation Maybe [ArgLiteral char]
