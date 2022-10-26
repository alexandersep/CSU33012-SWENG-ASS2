# CSU33012 - Software Engineering - Assignment 1
* Haskell implementation of Calculator for the Second Assignment.

## Brief Discription of the App
* The app was written in Haskell. 
    - Both the frontend and backend were written in Haskell. 
        - For the frontend we used a library called Lucid and Spock to create a html from haskell. 
        - For the backend we simply used Haskell with Hunit and QuickCheck for unit testing using `stack test`
    - The calculator took in operators `+-*/^el` where the operators are characters that do 
      addition, subtraction, multiplication, division, exp, and log respectively.
        - Algorithm implemented was the shutting yard algorithm to compute and infix expression to postfix and calculated the postfix expression.
        - The expression validator also uses a similar approach to the shutting yard algorithm to check whether an expression is infix and valid.
            - Operators `e` and `l` are prefix expressions. So a workaround was created which converted a single prefix argument
              of `l 5` to `0 l 5` where the first operand was ignored in its calculation. In a sense creating a prefix to infix converter.
        - Unary operators were implemented for `+-` on the case of numbers. This unary operator does not work on `l` or `e` however since they
          are not considered numbers in the calculator. Also if starting with brackets you cannot have a unary operator in the calculator e.g.
          `- - ( 1 + 2 )` is not allowed but `1 - - ( 1 + 2)` is allowed.
        - The answer will be returned to 3 decimal places as required.
        - The textbox does not take into carriage returns when inputting in the web app. Hence a correct answer will be given 
          if something like `3\n+\n\n\r\n2` is given. (The *\r* and or *\n* are inputted in the web app when pressing enter).
    - CI/CD
        - Continuous Integration will be triggered to run unit tests whenever anything is pushed to the main branch including a merge.
        - Continuos Delivery will be triggered when creating a new release tag and it will publish the docker image to dockerhub `asepelenco/interactive-calculator` 
            - Two releases are in that dockerhub, v1.0.1 (CLI) and v2.0.0 (GUI html). 
    - Releases
        - First release v1.0.1 is an implementation of the app before web app. This means it works in the terminal and would be considered a 
          command line interface (CLI) of the app
        - Second release v2.0.0 is the final implementation. No terminal support and works on the web with html. The web app runs in `localhost:8080` 
      

### Contributors & Contributions
* Alexander Sepelenco
    - Acted as github lead with organising, README, pull requests, 
      github workflow, and setting up Haskell with stack before the web app implementation. (Done in Assignment 1)
    - Set up Haskell unit testing: Hunit, and Quickcheck (Done in Assignment 1).
    - Set up github CI/CD workflow with caching and set up Dockerhub.
    * Haskell Related Contributions
        - All Haskell related contributions were taken from Assignment 1
          and expanded from with Assignment 2
        - Implemented the following functions and their respective unit tests
          `isOperator`, `iOperand`, `operatorPrecedence`, `errorPrecedence`,
          `isOperatorLeftAssociative`, `errorLeftAssociativity`, `removeSpaces`,
          `splitToList`, `addZeroStringUnaryHeadPositiveOrNegative`,
          `combineUnaryOperators`, `removeUnaryHeadPositive`, `removePlusNum`,
          `combineNum`, `addZeroExponent`, `countDots`, `isOperand'`. All functions including the ones I wrote
          now take into account the changes for this assignment, these changes are log, exp. Note that these functions were mostly
          written in Assignment 1 with a different group but had some additions and changes throughout the functions for Assignment 2.
        - Fixed Unary operators partially from Assignment 1 with edge case and parsing, `-` and `+` and ensured it worked effectively
          Unary operators do not work in l and e (log and exp). It does not work with brackets if at the beginning of a line.
          e.g. `-- (1 + 2)` does not work but `1 -- (1 + 2)` does.
* Elliot Lyons
    - TODO
* Annelee O Mahony
    - TODO
* Brian Sharkey
    - TODO

* Brian Sharkey
  * Collaborated with Elliot Lyons to develop a webapp for the calculator
    * Implemented the calculator into the web app
  * Contributed on the creation of documentaion
  * Contributed to creating the demo video

* Annelee O' Mahony
  * Collaborated with Alexander Sepelenco on the functions 
    * Edited the functions for improvement/expansion of the calculator
  * Package Management
  * Created and recorded the Demo video
  * Worked on the documentation


### Graph/commit logs
*TODO*

#### Link To repo
[https://github.com/alexandersep/CSU33012-SWENG-ASS2](https://github.com/alexandersep/CSU33012-SWENG-ASS2) 
