module LibSpec (spec) where

import Lib
    ( isOperator, isOperand,
      operatorPrecedence, errorPrecedence,
      isOperatorLeftAssociative, errorLeftAssociativity,
      infixValidator, infixValidator', splitToList, removeSpaces, 
      infixToPostfix, infixToPostfix', popRemaining, popOperatorStack,
      popOperatorStackUpToParen, getFirstElem,
      evaluatePostfix, evaluateExpression, combineUnaryOperators,
      removeUnaryHeadPositive, removePlusNum, addZeroStringUnaryHeadPositiveOrNegative,
      combineNum, countBrackets, addZeroExponent, countDots
      )
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "Validate function for addZeroExponent" $ do
        it "returns [\"5\"] for [\"5\"]" $ do
            addZeroExponent ["5"] `shouldBe` ["5"] 
        it "returns [\"0\",\"e\",\"(\"] for [\"e\",\"(\"]" $ do
            addZeroExponent ["e","("] `shouldBe` ["0","e","("] 
        it "returns [\"0\",\"e\",\"5\"] for [\"e\",\"5\"]" $ do
            addZeroExponent ["e","5"] `shouldBe` ["0","e","5"] 
        it "returns [\"1\",\"+\",\"0\",\"e\",\"5\"] for [\"1\",\"+\",\"e\",\"5\"]" $ do
            addZeroExponent ["1","+","e","5"] `shouldBe` ["1","+","0","e","5"] 
        it "returns [\"0\",\"l\",\"5\"] for [\"l\",\"5\"]" $ do
            addZeroExponent ["l","5"] `shouldBe` ["0","l","5"] 
        it "returns [\"1\",\"+\",\"0\",\"l\",\"5\"] for [\"1\",\"+\",\"l\",\"5\"]" $ do
            addZeroExponent ["1","+","l","5"] `shouldBe` ["1","+","0","l","5"] 

    describe "Validate function for isOperator" $ do
        it "returns True for isOperator 2" $ do
            isOperator '2' `shouldBe` False 
        it "returns False for isOperator *" $ do
            isOperator '*' `shouldBe` True 
        it "returns False for isOperator *" $ do
            isOperator '-' `shouldBe` True 
        it "returns False for isOperator *" $ do
            isOperator '+' `shouldBe` True 
        it "returns False for isOperator *" $ do
            isOperator '/' `shouldBe` True 
        it "returns False for isOperator *" $ do
            isOperator 'e' `shouldBe` True 
        it "returns False for isOperator *" $ do
            isOperator 'l' `shouldBe` True 


    describe "Validate function for countDots" $ do
        it "returns 3 for countDots \"1234.34.532.2\"" $ do
            countDots 0 "1234.34.532.2" `shouldBe` 3 
        it "returns 1 for countDots \"1234.34\"" $ do
            countDots 0 "1234.34" `shouldBe` 1 

    describe "Validate function for isOperand" $ do
        it "returns False for isOperand []" $ do
            isOperand [] `shouldBe` False 
        it "returns True for isOperand 2" $ do
            isOperand "2" `shouldBe` True
        it "returns False for isOperand *" $ do
            isOperand "*" `shouldBe` False
        it "returns True for isOperand 100" $ do
            isOperand "100" `shouldBe` True
        it "returns True for isOperand -1" $ do
            isOperand "-1" `shouldBe` True
        it "returns True for isOperand ------1" $ do
            isOperand "-----1" `shouldBe` False
        it "returns True for isOperand - ------1" $ do
            isOperand "- -----1" `shouldBe` False
        it "returns True for isOperand -1" $ do
            isOperand "-1.34.34" `shouldBe` True -- despite this seeming false isOperand takes in numbers that look real but may not be 
        it "returns True for isOperand 100" $ do
            isOperand "100.1234" `shouldBe` True

    describe "Validate function for operatorPrecedence" $ do
        it "returns Nothing for operatorPrecedence a" $ do
            operatorPrecedence 'a' `shouldBe` Nothing 
        it "returns Just 2 for operatorPrecedence -" $ do
            operatorPrecedence 'e' `shouldBe` Just 5 
        it "returns Just 2 for operatorPrecedence -" $ do
            operatorPrecedence 'l' `shouldBe` Just 5 
        it "returns Just 4 for operatorPrecedence ^" $ do
            operatorPrecedence '^' `shouldBe` Just 4 
        it "returns Just 3 for operatorPrecedence *" $ do
            operatorPrecedence '*' `shouldBe` Just 3 
        it "returns Just 3 for operatorPrecedence /" $ do
            operatorPrecedence '/' `shouldBe` Just 3 
        it "returns Just 2 for operatorPrecedence +" $ do
            operatorPrecedence '+' `shouldBe` Just 2
        it "returns Just 2 for operatorPrecedence -" $ do
            operatorPrecedence '-' `shouldBe` Just 2 

    describe "Validate function for errorPrecedence" $ do
        it "returns It has a precedence for operatorPrecedence Just 4" $ do
            errorPrecedence (Just 4) `shouldBe` "It has a precedence"
        it "returns Error, does not have associativity for operatorPrecedence Nothing" $ do
            errorPrecedence Nothing `shouldBe` "Error, does not have a precedence" 

    describe "Validate function for isOperatorLeftAssociative" $ do
        it "returns Nothing for isOperatorLeftAssociative a" $ do
            isOperatorLeftAssociative 'a' `shouldBe` Nothing
        it "returns Just False for isOperatorLeftAssociative ^" $ do
            isOperatorLeftAssociative '^' `shouldBe` Just False 
        it "returns Just True for isOperatorLeftAssociative +" $ do
            isOperatorLeftAssociative '+' `shouldBe` Just True
        it "returns Just True for isOperatorLeftAssociative -" $ do
            isOperatorLeftAssociative '-' `shouldBe` Just True
        it "returns Just True for isOperatorLeftAssociative *" $ do
            isOperatorLeftAssociative '*' `shouldBe` Just True
        it "returns Just True for isOperatorLeftAssociative /" $ do
            isOperatorLeftAssociative '/' `shouldBe` Just True

    describe "Validate function for errorLeftAssociativity" $ do
        it "returns Error, does not have associativity for errorLeftAssociativity Just True" $ do
            errorLeftAssociativity (Just True) `shouldBe` "It has an associativity" 
        it "returns Error, does not have associativity for errorLeftAssociativity Nothing" $ do
            errorLeftAssociativity Nothing `shouldBe` "Error, does not have associativity" 

    describe "Validate function for Infix Expressions" $ do
        it "returns False for []" $ do
          infixValidator [] `shouldBe` False
        it "returns False for [\"*\",\"3\"]" $ do
          infixValidator ["*","3"] `shouldBe` False
        it "returns False for [\"l\",\"3\"]" $ do -- do note that addZeroExponent prevents it from just being two values
          infixValidator ["l","3"] `shouldBe` False
        it "returns False for (30 + 5)2" $ do
          infixValidator ["(", "30", "+", "5", ")", "2"] `shouldBe` False
        it "returns True for (30 + 5) + 2" $ do
          infixValidator ["(", "30", "+", "5", ")", "+", "2"] `shouldBe` True
        it "returns invalid for (30 + 5) + )" $ do
          infixValidator ["(", "30", "+", "5", ")", "+", ")"] `shouldBe` False
        it "returns False for (30 + 5) + 2)" $ do
          infixValidator ["(", "30", "+", "5", ")", "+", "2", ")"] `shouldBe` False
        it "returns True for (1 + -1) + 2" $ do
          infixValidator ["(", "1", "+", "-1", ")", "+", "2"] `shouldBe` True
        it "returns True for (--1 + --2) + -2" $ do
          infixValidator ["(", "--1", "+", "--2", ")", "+", "-2"] `shouldBe` False
        it "returns False for () 1 + *) + 2" $ do
          infixValidator ["(", ")", "1", "+", "*", ")", "+", "2"] `shouldBe` False
        it "returns False for (* + /) + 2" $ do
          infixValidator ["(", "*", "+", "/", ")", "+", "2"] `shouldBe` False
        it "returns False for (- ------1 + 3)" $ do
          infixValidator ["(", "-", "------1", "+", "3", ")"] `shouldBe` False

    describe "Validate function for infixValidator'" $ do
        it "returns False for [\"&\"]" $ do
          infixValidator' ["&"] `shouldBe` False

    describe "Validate function for removeSpaces" $ do
        it "returns \"\" for \" \"" $ do
            removeSpaces " " `shouldBe` ""
        it "returns \"3+3342\" for \"3 + 334      2\"" $ do
            removeSpaces "3 + 334      2" `shouldBe` "3+3342"

    describe "Validate function for splitToList" $ do
        it "returns [] for []" $ do
            splitToList [] `shouldBe` []
        it "returns [] for \" \"" $ do
            splitToList "" `shouldBe` []
        it "returns [\"2\"] for \"   2   \"" $ do
            splitToList "   2    " `shouldBe` ["2"]
        it "returns [\"3\",\"+\",\"-\",\"3\",\"2\"] for \"     3 + -3 2  " $ do
            splitToList "        3 + -3 2   " `shouldBe` ["3","+","-","3","2"] 
        it "returns [\"2\",\"3\"] for \"   2   3\"" $ do
            splitToList "   2    3" `shouldBe` ["2","3"] 
        it "returns [\"23\",\"+4\",\"-\",\"-\",\"-\",\"34\",\"-\",\"434\",\"-\",\"-\",\"34\",\"+\",\"2\"] for \"23 +4 ---34 -434 --34 + 2   \"" $ do
            splitToList "23 +4 ---34 -434 --34 + 2   " `shouldBe` ["23","+","4","-","-","-","34","-","434","-","-","34","+","2"]
        it "returns [\"&\"] for \"&\"" $ do
            splitToList "&" `shouldBe` ["&"]
        it "returns [\"&\",\"4\"] for \"&4\"" $ do
            splitToList "&4" `shouldBe` ["&","4"]
    
    describe "Validate function for infixToPostfix" $ do
        it "returns [] for []" $ do
            infixToPostfix [] `shouldBe` []
        it "returns [\"3\", \"1\", \"+\"] for [\"3\", \"+\", \"1\"]" $ do
            infixToPostfix ["3", "+", "1"] `shouldBe` ["3", "1", "+"]
        it "returns [\"3\", \"1\", \"+\"] for [\"(\", \"3\", \"+\", \"1\", \")\"]" $ do
            infixToPostfix ["(", "3", "+", "1", ")"] `shouldBe` ["3", "1", "+"]
        it "returns [\"3\", \"1\", \"+\", \"4\", \"*\"] for [\"(\", \"3\", \"+\", \"1\", \")\", \"*\", \"4\"]" $ do
            infixToPostfix ["(", "3", "+", "1", ")", "*", "4"] `shouldBe` ["3","1","+","4","*"]
        it "returns [\"3\", \"1\", \"+\", \"4\", \"*\"] for [\"3\", \"+\", \"(\" \"1\", \"*\", \"4\", \")\"]" $ do
            infixToPostfix ["3", "*", "(", "1", "+", "4", ")"] `shouldBe` ["3","1","4","+","*"]
        it "returns [\"3\", \"1\", \"4\", \"/\", \"^\"] for [\"3\", \"^\", \"1\", \"/\", \"4\"]" $ do
            infixToPostfix ["3", "^", "1", "/", "4"] `shouldBe` ["3","1","^","4","/"]
        it "returns [\"3\", \"1\", \"*\", \"4\", \"^\"] for [\"(\", \"(\", \"3\", \"*\", \"1\", \")\", \"^\", \"4\", \")\"]" $ do
            infixToPostfix ["(", "(", "3", "*", "1", ")", "^", "4", ")"] `shouldBe` ["3","1","*","4","^"]

    describe "Validate function for infixToPostfix'" $ do
        it "returns ([\"5\"], [], []) for ([\"5\"], [], [])" $ do
            infixToPostfix' (["5"],[],[]) `shouldBe` (["5"],[],[]) 
    
    describe "Validate function for popRemaining" $ do
        it "returns ([], [], []) for ([], [], [])" $ do
            popRemaining ([], [], []) `shouldBe` ([], [], [])
        it "returns ([\"+\"], [], [\"4\"]) for ([], [\"+\"], [\"4\"])" $ do
            popRemaining ([], ["+"], ["4"]) `shouldBe` (["+"], [], ["4"])
        it "returns ([\"3\", \"4\", \"+\", \"-\"], [], [\"4\"]) for ([\"3\", \"4\",], [\"+\", \"-\"], [\"4\"])" $ do
            popRemaining (["3", "4"], ["+", "-"], ["4"]) `shouldBe` (["3", "4", "+", "-"], [], ["4"])
        
    describe "Validate function for popOperatorStack" $ do
        it "returns ([\"3\"], [\"^\",\"^\"], [\"5\"]) for ([], [], []) \"^\"" $ do
            popOperatorStack (["3"], ["^","^"], ["5"]) "^" `shouldBe` (["3"], ["^","^","^"], ["5"])
        it "returns ([], [\"+\"], []) for ([], [], []) \"+\"" $ do
            popOperatorStack ([], [], []) "+" `shouldBe` ([], ["+"], []) 
        it "returns ([\"3\"], [], [\"4\"]) for ([\"3\"], [\")\"], [\"4\"]) \"+\"" $ do
            popOperatorStack (["3"], [")"], ["4"]) "+" `shouldBe` (["3"], ["+", ")"], ["4"])
        it "returns ([\"3\", \"-\", \"+\"], [], [\"4\"]) for ([\"3\"], [\"-\", \"+\", \")\"], [\"4\"]) \"*\"" $ do
            popOperatorStack (["3"], ["-", "+", ")"], ["4"]) "*" `shouldBe` (["3"],["*","-","+",")"],["4"])
        it "returns ([\"3\"], [\"*\", \"/\", \")\"], [\"4\"]) for ([\"3\", \"*\", \"/\"], [\"+\", \")\"], [\"4\"]) \"+\"" $ do
            popOperatorStack (["3"], ["*", "/", ")"], ["4"]) "+" `shouldBe` (["3", "*", "/"],["+",")"],["4"])
    
    describe "Validate function for popOperatorStackUpToParen" $ do
        it "returns ([], [], []) for ([], [\"(\")], [])" $ do
            popOperatorStackUpToParen ([], ["("], []) `shouldBe` ([], [], [])
        it "returns ([], [], []) for ([], [], [])" $ do
            popOperatorStackUpToParen ([], [], []) `shouldBe` ([], [], []) 
        it "returns ([\"+\", \"-\"], [], []) for ([], [\"+\", \"-\", \"(\")], [])" $ do
            popOperatorStackUpToParen ([], ["+", "-", "("], []) `shouldBe` (["+", "-"], [], [])
    
    describe "Validate function for getFirstElem" $ do
        it "returns ([], [], []) for ([], [], [])" $ do
            getFirstElem ([], ["3"], []) `shouldBe` []
        it "returns [\"+\"] for ([\"+\"], [\"-\"], [\"/\"])" $ do
            getFirstElem (["+"], ["-"], ["/"]) `shouldBe` ["+"]

    describe "Validate function for evaluateExpression" $ do
        it "returns 1.0986123 for 0 l 3" $ do -- 0 is just filler that's needed since we pretend it's an infix expression
            evaluateExpression 0 "l" 3 `shouldBe` 1.0986123 
        it "returns 20.085537 for 0 e 3" $ do -- 0 is just filler that's needed since we pretend it's an infix expression
            evaluateExpression 0 "e" 3 `shouldBe` 20.085537 
        it "returns 6 for 3 * 2" $ do
            evaluateExpression 3 "*" 2 `shouldBe` 6 
        it "returns 9 for 18 / 2" $ do
            evaluateExpression 18 "/" 2 `shouldBe` 9
        it "returns 5 for 3 + 2" $ do
            evaluateExpression 3 "+" 2 `shouldBe` 5
        it "returns 1 for 3 - 2" $ do
            evaluateExpression 3 "-" 2 `shouldBe` 1
        it "returns 9 for 3 ^ 2" $ do
            evaluateExpression 3 "^" 2 `shouldBe` 9

    describe "Validate function for evaluatePostfix" $ do
        it "returns Nothing for []" $ do
            evaluatePostfix [] `shouldBe` Nothing
        it "returns Just 5 for [\"3\" \"2\" \"+\"]" $ do
            evaluatePostfix ["3", "2", "+"] `shouldBe` Just 5
        it "returns Just 20 for [\"3\" \"2\" \"+\", \"4\", \"*\"]" $ do
            evaluatePostfix ["3", "2", "+", "4", "*"] `shouldBe` Just 20
        it "returns Just -1.5 for [\"3\" \"2\" \"4\", \"-\", \"/\"]" $ do
            evaluatePostfix ["3", "2", "4", "-", "/"] `shouldBe` Just (-1.5)
        it "returns Just X for [\"3\" \"2\" \"4\", \"-\", \"*\", \"5\", \"^\"]" $ do
            evaluatePostfix ["3", "2", "+", "4", "*", "5", "^"] `shouldBe` Just 3200000 

    describe "Validate function for removePlusNum" $ do
        it "returns [] for []" $ do
            removePlusNum [] `shouldBe` []
        it "returns [\"+\"] for [\"+\"]" $ do
            removePlusNum ["+"] `shouldBe` ["+"]
        it "returns [\"+\",\"3\"] for [\"+\",\"3\"]" $ do
            removePlusNum ["+","3"] `shouldBe` ["+","3"]
        it "returns [\"3\",\"+\",\"(\",\"3\",\")\"] for [\"3\",\"+\",\"(\",\"+\",\"3\",\")\"]" $ do
            removePlusNum ["3","+","(","+","3",")"] `shouldBe` ["3","+","(","3",")"]


    describe "Validate function for addZeroStringUnaryHeadPositiveOrNegative" $ do
        it "returns [] for []" $ do
            addZeroStringUnaryHeadPositiveOrNegative [] `shouldBe` []
        it "returns [\"-\"] for [\"-\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-"] `shouldBe` ["-"]
        it "returns [\"0\",\"-\",\"(\"] for [\"-\",\"(\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-","("] `shouldBe` ["0","-","("]
        it "returns [\"0\",\"-\",\"e\"] for [\"-\",\"e\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-","e"] `shouldBe` ["0","-","e"]
        it "returns [\"0\",\"-\",\"l\"] for [\"-\",\"l\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-","l"] `shouldBe` ["0","-","l"]
        it "returns [\"-\",\")\"] for [\"-\",\")\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-",")"] `shouldBe` ["-",")"]
        it "returns [\"+\"] for [\"+\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["+"] `shouldBe` ["+"]
        it "returns [\"0\",\"+\",\"3\"] for [\"+\",\"3\"]" $ do 
            addZeroStringUnaryHeadPositiveOrNegative ["+","3"] `shouldBe` ["0","+","3"]
        it "returns [\"-3\"] for [\"-3\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-3"] `shouldBe` ["-3"]
        it "returns [\"/\",\"-\"] for [\"/\",\"-\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["/","-"] `shouldBe` ["/","-"]
        it "returns [\"0\",\"-\",\"-3\"] for [\"-\",\"-3\"]" $ do
            addZeroStringUnaryHeadPositiveOrNegative ["-","-3"] `shouldBe` ["0","-","-3"]

    describe "Validate function for removeUnaryHeadPositive" $ do
        it "returns [] for []" $ do
            removeUnaryHeadPositive [] `shouldBe` []
        it "returns [] for [\"+\"]" $ do
            removeUnaryHeadPositive ["+"] `shouldBe` []
        it "returns [\"3\"] for [\"+\",\"3\"]" $ do 
            removeUnaryHeadPositive ["+","3"] `shouldBe` ["3"]
        it "returns [\"-\",\"3\"] for [\"-\",\"3\"]" $ do
            removeUnaryHeadPositive ["-","3"] `shouldBe` ["-","3"]
        
    describe "Validate function for combineUnaryOperators" $ do
        it "returns [] for []" $ do
            combineUnaryOperators [] `shouldBe` []            
        it "returns [\"0\",\"-\",\"-3\",\"*\",\"2\",\"+\",\"(\",\"-4\",\"/\",\"2\",\")\",\"**\",\"2\",\"-\",\"(\",\"1\",\"-\",\"(\",\"1\",\"+\",\"£\",\"-\",\"$\"] for  [\"0\",\"-\",\"-3\",\"*\",\"2\",\"+\",\"(\",\"-4\",\"/\",\"2\",\")\",\"**\",\"2\",\"-\",\"(\",\"1\",\"-\",\"(\",\"1\",\"+\",\"£\",\"-\",\"$\"]" $ do
            combineUnaryOperators ["0","-","-3","*","2","+","(","-4","/","2",")","**","2","-","(","1","-","(","1","+","£","-","$"] `shouldBe` ["0","-","-3","*","2","+","(","-4","/","2",")","**","2","-","(","1","-","(","1","+","£","-","$"]
        it "returns [\"-\",\"&\"] for [\"-\",\"&\"]" $ do
            combineUnaryOperators ["-","&"] `shouldBe` ["-","&"]
        it "returns [\"-\"] for [\"-\"]" $ do
            combineUnaryOperators ["-"] `shouldBe` ["-"]
        it "returns [\"-\",\"3\"] for [\"-\",\"3\"]" $ do
            combineUnaryOperators ["-","3"] `shouldBe` ["-","3"]
        it "returns [\"-\",\"3\"] for [\"+\",\"-\",\"3\"]" $ do
            combineUnaryOperators ["+","-","3"] `shouldBe` ["-","3"]
        it "returns [\"+\"] for [\"+\"]" $ do 
            combineUnaryOperators ["+"] `shouldBe` ["+"]
        it "returns [\"-\"] for [\"-\",\"+\"]" $ do
            combineUnaryOperators ["-","+"] `shouldBe` ["-"]
        it "returns [\"-\"] for [\"+\",\"-\"]" $ do 
            combineUnaryOperators ["+","-"] `shouldBe` ["-"]
        it "returns [\"-\"] for [\"-\",\"-\"]" $ do
            combineUnaryOperators ["-","-"] `shouldBe` ["+"]
        it "returns [\"-\"] for [\"+\",\"+\"]" $ do
            combineUnaryOperators ["+","+"] `shouldBe` ["+"]
        it "returns [\"-\",\"^\",\"+\",\"/\",\"-\",\"*\",\"3\",\"/\",\"+\",\"4\"] for [\"-\",\"+\",\"^\",\"+\",\"+\",\"-\",\"-\",\"/\",\"-\",\"*\",\"3\",\"/\",\"-\",\"-\",\"+\",\"4\"]" $ do
            combineUnaryOperators ["-","+","^","+","+","-","-","/","-","*","3","/","-","-","+","4"] `shouldBe`  ["-","^","+","/","-","*","3","/","+","4"] 

    describe "Validate function for combineNum" $ do
        it "returns [] for []" $ do
            combineNum [] `shouldBe` []            
        it "returns [\"-\"] for [\"+\"]" $ do
            combineNum ["+"] `shouldBe` ["+"]
        it "returns [\"-\"] for [\"-\",\"3\"]" $ do
            combineNum ["-","3"] `shouldBe` ["-","3"]
        it "returns [\"+\",\"3\"] for [\"+\",\"3\"]" $ do
            combineNum ["+","3"] `shouldBe` ["+","3"]
        it "returns [\"+\",\"3\"] for [\"+\",\"3\"]" $ do
            combineNum ["+","3"] `shouldBe` ["+","3"]
        it "returns [\"234.234\"] for [\"234\",\".\",\"234\"]" $ do
            combineNum ["234",".","234"] `shouldBe` ["234.234"]
        it "returns [\"0\",\"-\",\"3\",\"*\",\"2\",\"/\",\"5\",\"+\",\"1\",\"-\",\"(\",\"(\",\"-1\",\"-\",\"0\",\")\",\"-\",\"(\",\"-1\",\"+\",\"2\",\")\",\")\",\"*\",\"4\"] for [\"0\",\"-\",\"3\",\"*\",\"2\",\"/\",\"5\",\"+\",\"1\",\"-\",\"(\",\"(\",\"-\",\"1\",\"-\",\"0\",\")\",\"-\",\"(\",\"-\",\"1\",\"+\",\"2\",\")\",\")\",\"*\",\"4\"]" $ do
            combineNum ["0","-","3","*","2","/","5","+","1","-","(","(","-","1","-","0",")","-","(","-","1","+","2",")",")","*","4"] `shouldBe`["0","-","3","*","2","/","5","+","1","-","(","(","-1","-","0",")","-","(","-1","+","2",")",")","*","4"] 
    
    describe "Validate function for countBrackets" $ do
        it "returns false for [] 0 1" $ do
            countBrackets [] 0 1 `shouldBe` False
        it "returns true for [] 0 0" $ do
            countBrackets [] 0 0 `shouldBe` True
        it "returns true for [\"(\", \"(\", \")\", \")\"]" $ do
            countBrackets ["(", "(", ")", ")"] 0 0 `shouldBe` True
        it "returns false for [\"(\", \"(\", \")\", \")\"]" $ do
            countBrackets ["(", "(", ")"] 0 0 `shouldBe` False
        it "returns true for [\"(\", \"3\", \"(\", \"^\", \")\", \"+\", \")\"]" $ do
            countBrackets ["(", "3", "(", "^", ")", "+", ")"] 0 0 `shouldBe` True
        it "returns false for [\"(\", \"3\", \"(\", \"^\", \")\", \"+\"]" $ do
            countBrackets ["(", "3", "(", "^", ")", "+"] 0 0 `shouldBe` False
