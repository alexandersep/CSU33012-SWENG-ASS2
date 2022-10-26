# CSU33012-SWENG-ASS2
This Haskell implementation was mostly done in Assignment 1 of SWENG found in 
[Assignment 1](https://github.com/alexandersep/CSU33012-SWENG-ASS2.git). Most of the calculator logic code
was written in Assignment 1 by Alexander Sepelenco sepelena@tcd.ie and Niall Sauvage sauvagen@tcd.ie.
Haskell Implementation of Unit testing a basic calculator, All other contributions (contributions towards
the second assignment as seen with the git commits were done by Alexander Sepelenco, sepelena@tcd.ie,
Elliot Lyons elliot243lyons@gmail.com, Annelee O Mahony omahona5@tcd.ie, and Brain Sharkey bsharkey@tcd.ie \\

## Instructions for Setup
* Running App from Dockerhub
    1. Pull docker image of web app.
        * Option 1: Pull web app version using `docker pull asepelenco/interactive-calculator:v2.0.0`.
        * Option 2: Pull command line interface version using `docker pull asepelenco/interactive-calculator:v1.0.1`.
    2. Run the docker image using `docker run --expose 8080 -p 8080:8080 -ti --rm asepelenco/interactive-calculator:v2.0.0`.
        * `--expose 8080` and `-p 8080:8080` ensures the port `8080` is exposed and allows you to access the web
          with the docker image.
    3. Open up your web browser and type `localhost:8080`, and click enter.
* Running App using Docker (not from Dockerhub) (This can only be the web app version)
    1. Execute command `docker build -t haskell_calc .` in root directory of the project (CSU33012-SWENG-ASS2)
    2. Execute command `docker run --expose 8080 -p 8080:8080 -ti --rm haskell_calc` (Exposing port is not strictly necessary
       if running docker from host machine).
    3. Open up your web browser and type `localhost:8080`, and click enter.
* Running App locally
    1. Clone repository with `git clone https://github.com/alexandersep/CSU33012-SWENG-ASS2.git`.
    2. Enter CSU33012-SWENG-ASS2/ with `cd CSU33012-SWENG-ASS2/`.
    3. Build the stack `stack build`.
    4. Compile test folder with `stack test` (Optional).
    5. Give yourself executable permissions for the `run.sh` script with `chmod +x scripts/run.sh`.
    6. Run Haskell programme with `./run.sh`.

## Calculator Usage & Examples 
* Running App from using Web
    * Enter expressions into the box provided into the web app and click calculate when you want the answer.
* Running App from terminal
    * Enter expressions in terminal and click enter to evaluate the answer.
* Examples of command line release (v1.0.1) (works similarly in the box on the web app)
    * Inputs include brackets, negative operands, operands, operators "+-/\*^" and unary operators "+-"
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
Elliot Lyons, elliot243lyons@gmail.com \
Annelee O Mahony, omahona5@tcd.ie \
Brain Sharkey, bsharkey@tcd.ie
