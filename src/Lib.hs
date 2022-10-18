module Lib
    ( isOperator, isOperand,
      operatorPrecedence,
      isOperatorLeftAssociative,
      infixValidator, infixValidator', popOperatorStackUpToParen,
      infixToPostfix', countBrackets,
      errorPrecedence, errorLeftAssociativity,
      splitToList, removeSpaces,
      infixToPostfix, popRemaining,
      popOperatorStack, getFirstElem,
      evaluatePostfix, evaluateExpression, 
      removeUnaryHeadPositive, combineUnaryOperators,
      addZeroStringUnaryHeadPositiveOrNegative, removePlusNum,
      combineNum, addZeroExponent
    ) where

import Data.Char (isSpace, isNumber, isDigit) 

-- Source: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
addZeroExponent :: [String] -> [String]
addZeroExponent [] = []
addZeroExponent [x] = [x]
addZeroExponent (x:y:xs)
 | (x == "e" || x == "l") && (isOperand y || y == "(") = "0":x:y : addZeroExponent xs
 | otherwise = x : addZeroExponent (y:xs)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^el"

-- non negative operand, operands in our case are non negative
isOperand :: String -> Bool
isOperand "-" = False
isOperand ('-':xs) = isOperand' xs
isOperand xs  = isOperand' xs && (countDots 0 xs <= 1)

isOperand' :: String -> Bool
isOperand' []     = False
isOperand' [x]    = isDigit x
isOperand' (x:xs) = isDigit x || x == '.' && isOperand' xs

-- countDots, dots -> string -> true if 1 or less dots
countDots :: Int -> String -> Int
countDots d [] = d 
countDots d (x:xs)
 | x == '.'  = countDots (d+1) xs  
 | otherwise = countDots d xs

operatorPrecedence :: Char -> Maybe Int
operatorPrecedence x
 | x == 'e' || x == 'l' = Just 5
 | x == '^'             = Just 4
 | x == '*' || x == '/' = Just 3
 | x == '+' || x == '-' = Just 2
 | otherwise            = Nothing

errorPrecedence :: Maybe Int -> String
errorPrecedence x =
    case x of
        Just _  -> "It has a precedence"
        Nothing -> "Error, does not have a precedence"

isOperatorLeftAssociative :: Char -> Maybe Bool
isOperatorLeftAssociative x
 | x == '^'     = Just False
 | isOperator x = Just True
 | otherwise    = Nothing

errorLeftAssociativity :: Maybe Bool -> String
errorLeftAssociativity x =
    case x of
        Just _  -> "It has an associativity"
        Nothing -> "Error, does not have associativity"

infixValidator :: [String] -> Bool
infixValidator xs = infixValidator' xs && countBrackets xs 0 0

infixValidator' :: [String] -> Bool
infixValidator' [] = False
infixValidator' [")"] = True
infixValidator' [x]
 | isOperand x = True
 | otherwise   = False 
infixValidator' (x:xs)
 | isOperator (head x) && (firstOperandElem || head xs == "(") = infixValidator' xs
 | isOperand x        && (firstOperatorElem || head xs == ")") = infixValidator' xs
 | x == ")"           && (firstOperatorElem || head xs == ")") = infixValidator' xs
 | x == "("           && (firstOperandElem  || head xs == "(") = infixValidator' xs
 | otherwise = False
 where firstOperandElem  = isOperand . head  $ xs
       firstOperatorElem = isOperator . head . head $ xs

countBrackets :: [String] -> Int -> Int -> Bool
countBrackets [] open close = open == close
countBrackets (x:xs) open close
 | x == "("  = countBrackets xs (open+1) close
 | x == ")"  = countBrackets xs open (close+1)
 | otherwise = countBrackets xs open close

addZeroStringUnaryHeadPositiveOrNegative :: [String] -> [String]
addZeroStringUnaryHeadPositiveOrNegative [] = []
addZeroStringUnaryHeadPositiveOrNegative [x] = [x]
addZeroStringUnaryHeadPositiveOrNegative (x:y:xs)
 | x == "-" && isOperand y = "0":x:y:xs 
 | x == "-" && (y == "(" || y == "e" || y == "l") = "0":x:y:xs
 | x == "+"  = "0":x:y:xs
 | otherwise = x:y:xs

removePlusNum :: [String] -> [String]
removePlusNum [] = []
removePlusNum [x] = [x]
removePlusNum [x,y] = x : [y] 
removePlusNum (x:y:z:xs)
 | notOperandX && x /= ")" && plusRule = x : z : removePlusNum xs
 | otherwise               = x : removePlusNum (y:z:xs)
 where notOperandX = not $ isOperand x
       plusRule    = y == "+" && isOperand z

removeUnaryHeadPositive :: [String] -> [String]
removeUnaryHeadPositive [] = []
removeUnaryHeadPositive (x:xs)
 | x == "+" = xs
 | otherwise = x:xs

combineUnaryOperators :: [String] -> [String]
combineUnaryOperators [] = []
combineUnaryOperators [x] = [x]
combineUnaryOperators (x:y:xs)
 | isOperand x  = x : combineUnaryOperators (y:xs)
 | minusRule    = combineUnaryOperators ("-":xs)
 | plusRule     = combineUnaryOperators ("+":xs)
 | y /= "+" && y /= "-" = x : y : combineUnaryOperators xs
 | otherwise    =  x : combineUnaryOperators (y:xs)
 where minusRule   = x == "+" && y == "-" || x == "-" && y == "+"
       plusRule    = x == "+" && y == "+" || x == "-" && y == "-"

combineNum :: [String] -> [String]
combineNum [] = []
combineNum [x] = [x]
combineNum (x:y:[]) = x : [y]
combineNum (x:y:z:xs)
 | not (isOperand x) && x /= ")" &&
    (y == "+" || y == "-") && 
    isOperand z = x : (y ++ z) : combineNum xs
 | isOperand x && y == "." && isOperand z = (x ++ y ++ z) : combineNum xs
 | otherwise    = x : combineNum (y:z:xs)

splitToList :: String -> [String]
splitToList [] = []
splitToList (x:xs)
 | x == ' '            = splitToList xs
 | not (isOperand [x]) = [x] : splitToList xs
 | otherwise           = fstSpan : sndSpan 
 where spanNum = span isNumber xs
       fstSpan = x : fst spanNum
       sndSpan = splitToList . snd $ spanNum

removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

infixToPostfix :: [String] -> [String]
infixToPostfix [] = []
infixToPostfix xs = getFirstElem $ infixToPostfix' ([], [], xs)

infixToPostfix' :: ([String], [String], [String]) -> ([String], [String], [String])
infixToPostfix' (xs, [], []) = (xs, [], [])
infixToPostfix' (xs, ys, []) = infixToPostfix' $ popRemaining (xs, ys, [])
infixToPostfix' (xs, ys, z:zs)
 | isOperand z         = infixToPostfix' (xs ++ [z], ys, zs)
 | z == "("            = infixToPostfix' (xs, z:ys, zs)
 | isOperator (head z) = infixToPostfix' $ popOperatorStack (xs, ys, zs) z
 | z == ")"            = infixToPostfix' $ popOperatorStackUpToParen (xs, ys, zs)

getFirstElem :: ([a], [a], [a]) -> [a]
getFirstElem (x, _, _) = x

popOperatorStack :: ([String], [String], [String]) -> String -> ([String], [String], [String])
popOperatorStack (xs, [],  zs) op = (xs, [op], zs)
popOperatorStack (xs, y:ys, zs) op
 | isOperator (head y) &&
    ((operatorPrecedence (head y) > operatorPrecedence (head op)) 
    || (operatorPrecedence (head y) == operatorPrecedence (head op) && op /= "^")) = popOperatorStack (xs ++ [y], ys, zs) op
 | otherwise = (xs, op:y:ys, zs)

popOperatorStackUpToParen :: ([String], [String], [String]) -> ([String], [String], [String])
popOperatorStackUpToParen (xs, [], zs) = (xs, [], zs)
popOperatorStackUpToParen (xs, y:ys, zs)
 | y /= "("  = popOperatorStackUpToParen (xs ++ [y], ys, zs)
 | otherwise = (xs, ys, zs)

popRemaining :: ([String], [String], [String]) -> ([String], [String], [String])
popRemaining (xs, [], zs)   = (xs, [], zs)
popRemaining (xs, y:ys, zs) = popRemaining (xs ++ [y], ys, zs)

evaluatePostfix :: [String] ->  Maybe Float
evaluatePostfix [] = Nothing
evaluatePostfix xs = Just $ evaluatePostfix' xs []

evaluatePostfix' :: [String] -> [Float] -> Float
evaluatePostfix' [] [ys]     = ys
evaluatePostfix' (x:xs) []   = evaluatePostfix' xs [read x :: Float]
evaluatePostfix' (x:xs) [ys] = evaluatePostfix' xs $ (read x :: Float):[ys]
evaluatePostfix' (x:xs) (y1:y2:ys)
 | isOperand x = evaluatePostfix' xs $ (read x :: Float):y1:y2:ys
 | otherwise   = evaluatePostfix' xs $ evaluateExpression y2 x y1:ys

evaluateExpression :: Float -> String -> Float -> Float
evaluateExpression op1 opr op2
 | opr == "+"   = op1 +  op2
 | opr == "*"   = op1 *  op2
 | opr == "/"   = op1 /  op2
 | opr == "-"   = op1 -  op2
 | opr == "^"   = op1 ** op2
 | opr == "e"   = exp op2
 | opr == "l"   = log op2
 | otherwise = error "Invalid string provided as operator"
