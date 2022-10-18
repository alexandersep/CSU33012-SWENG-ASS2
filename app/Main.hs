module Main where

import Lib
import System.IO
import Text.Printf

main :: IO ()
main = do
    printf "Haskell infix Calculator, Operators include \"+-*^le\", Note: Input without Qutoes e.g. 2 + 3 instead of \"2 + 3\"\n"
    printf "This calculator computes unary operators\n" 
    printf "Example of complex query with unary operators: - ( -3 + - -+ + 4 ) * l 10 - e(4.2) + 1.1234 * 34.343 / - e 1.4\n"
    printf "Please input an infix expression: "
    hFlush stdout -- flush standard output explicitly in order to run getLine in the same line as putStr
    infixExpr <- getLine
    let splitInfixExrp = combineNum . removePlusNum . addZeroExponent . 
                         addZeroStringUnaryHeadPositiveOrNegative . removeUnaryHeadPositive . 
                         combineUnaryOperators . splitToList $ infixExpr
    let isInfixValid = infixValidator $ splitInfixExrp 
    let infixCalculation = evaluatePostfix . infixToPostfix $ splitInfixExrp 
    if isInfixValid
        then do 
                case infixCalculation of -- only will ever be Just x
                    (Just x)  -> printf "The answer is %.3g\n" x -- round to 3 decimal places and return x
        else printf "Invalid, the answer cannot be calculated\n" -- Nothing
    return ()
