

module BuiltInFunctions
    ( anyOfLiteral
    , noneOfLiteral
    , maybeLiteral
    ) where

import           ProgramAst

anyOfLiteral :: String -> FuncInvocation
anyOfLiteral string = BuiltInFuncInvocation1Arg AnyOf [ArgLiteral string]

noneOfLiteral :: String -> FuncInvocation
noneOfLiteral string = BuiltInFuncInvocation1Arg NoneOf [ArgLiteral string]

maybeLiteral :: String -> FuncInvocation
maybeLiteral char = BuiltInFuncInvocation1Arg Maybe [ArgLiteral char]
