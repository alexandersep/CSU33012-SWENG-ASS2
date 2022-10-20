# CSU33012 - Software Engineering - Assignment 1
* Infix validator and calculator written in **Haskell** using *Hunit*, *QuckCheck* 
  for unit testing and github workflow actions.

## Contributors & Contributions
* Alexander Sepelenco
    - Acted as github lead with organising, README, issues, pull requests, 
      github workflow, and setting up Haskell with stack.
    - Set up Haskell unit testing: Hunit, and Quickcheck.
    - Set up github including github workflow with caching.
    * Haskell Related Contributions
        - All Haskell related contributions were taken from Assignment 1
          and expanded from with Assignment 2
        - Implemented the following functions and their respective unit tests
          `isOperator`, `iOperand`, `operatorPrecedence`, `errorPrecedence`,
          `isOperatorLeftAssociative`, `errorLeftAssociativity`, `removeSpaces`,
          `splitToList`, `addZeroStringUnaryHeadPositiveOrNegative`,
          `combineUnaryOperators`, `removeUnaryHeadPositive`, `removePlusNum`,
          `combineNum`, `addZeroExponent`, `countDots`, `isOperand'`. All functions including the ones I wrote
          now take into account the changes for this assignment, these changes are 
          log, exp.
        - Implemented the basic Input and Output when running programme. 
        - Fixed Unary from Assignment 1 with edge case and 
          parsing, `-` and `+` and ensured it worked effectively
          with Niall's evaluator, and validators. Unary operators do not work in l and e (log() and exp()), and act funny with brackets

### Graph/commit logs
*TODO*

#### Link To repo
[https://github.com/alexandersep/CSU33012-SWENG-ASS2](https://github.com/alexandersep/CSU33012-SWENG-ASS2) 
