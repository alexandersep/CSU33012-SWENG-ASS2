# CSU33012-SWENG-ASS1
This Haskell implementation was mostly done in Assignment 1 of SWENG found in 
[Assignment 1](https://github.com/alexandersep/CSU33012-SWENG-ASS2.git). Most of the code
was written in Assignment 1 by Alexander Sepelenco sepelena@tcd.ie and Niall Sauvage sauvagen@tcd.ie.
Haskell Implementation of Unit testing a basic calculator \\

## Instructions for Setup
1. Clone repository with `git clone https://github.com/alexandersep/CSU33012-SWENG-ASS2.git`
2. Enter CSU33012-SWENG-ASS2/ with `cd CSU33012-SWENG-ASS2/`
3. Initialise the stack with `stack init` 
4. Compile test folder with `stack test` 
5. Give yourself executable permissions for the `run.sh` script with `chmod +x run.sh`
6. Run Haskell programme with `./run.sh`.

## Calculator Usage & Examples 
* Inputs include brackets, negative operands, operands, operators "+-/\*^" and unary operators "+-".
  Operands do not work 
```bash
./run.sh
Haskell infix Calculator, Operators include "+-*^le", Note: Input without Qutoes e.g. 2 + 3 instead of "2 + 3"
This calculator computes unary operators
Example of complex query with unary operators: - ( -3 + - -+ + 4 ) * l 10 - e(4.2) + 1.1234 * 34.343 / e 1.4
Please input an infix expression: -3 *-2 / 5 + 1 - ((1 - 0) - ( -1 --2))*4
The answer is: 2.2

./run.sh
Haskell infix Calculator, Operators include "+-*^le", Note: Input without Qutoes e.g. 2 + 3 instead of "2 + 3"
This calculator computes unary operators
Example of complex query with unary operators: - ( -3 + - -+ + 4 ) * l 10 - e(4.2) + 1.1234 * 34.343 / e 1.4
Please input an infix expression: --- -+ + - + -- 3 *+-+-2 / -- 5 + 1 - (  (- 1 - 0) - ( -1 --2)  )*  4
The answer is: 7.8

./run.sh
Haskell infix Calculator, Operators include "+-*^le", Note: Input without Qutoes e.g. 2 + 3 instead of "2 + 3"
This calculator computes unary operators
Example of complex query with unary operators: - ( -3 + - -+ + 4 ) * l 10 - e(4.2) + 1.1234 * 34.343 / e 1.4
Please input an infix expression: 3+5*e(4.2)/(5+7) 
The answer is: 30.786 


./run.sh
Haskell infix Calculator, Operators include "+-*^le", Note: Input without Qutoes e.g. 2 + 3 instead of "2 + 3"
This calculator computes unary operators
Example of complex query with unary operators: - ( -3 + - -+ + 4 ) * l 10 - e(4.2) + 1.1234 * 34.343 / e 1.4
Please input an infix expression: ++-- - (   3 + -4 * e 4.2 / 4.234 - - + 4 * 1 - - 0 + 2 * l 1 + e    1 ) 
The answer is 53.282
```

### Report with Contributions
* The report of contributors and who contributed what can be found 
  in the file `Contribution.md` as text or `Contribution.pdf` as a pdf.

#### Contributors
Alexander Sepelenco, sepelena@tcd.ie \
Elliot Lyons, elliot243lyons@gmail.com or lyonse@tcd.ie \
Annelee O Mahony, omahona5@gmail.com \
Brain Sharkey, *TODO*
