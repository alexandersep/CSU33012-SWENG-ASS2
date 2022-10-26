{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Printf
import System.IO
import Lib
import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Semigroup ((<>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Text (Text, unpack, pack)
import Data.IORef
import Web.Spock.Lucid (lucid)
import Lucid

data MySession = EmptySession
type Server a = SpockM () () ServerState a
newtype ServerState = ServerState { expression :: IORef Expression}
data Expression = Expression { contents :: Text }

app :: Server ()
app = do
  get root $ do
    expression' <- getState >>= (liftIO . readIORef . expression)
    lucid $ do
      h1_ "Haskell infix Calculator" 
      p_ "Operators include \"+-*^le\", Note: l, e are log and exponent respectively, Input without Qutoes e.g. 2 + 3 instead of \"2 + 3\"\n"
      p_ "This calculator computes unary operators, (Does not work with exp and log even though they are technically numbers and not operators e.g. - e(4.2) is invalid" 
      p_ "Example of complex query with unary operators: - ( -3 + - -+ + 4 ) * l 10 - e(4.2) + 1.1234 * 34.343 / e 1.4"
      form_ [method_ "post"] $ do
        label_ $ do
          "Mathematical Expression: "
          textarea_ [name_ "Mathematical Expression"] ""
        input_ [type_ "submit", value_ "calculate", name_ "result"]
      form_ [method_ "text"] $ do
        label_ $ do
            output_ . toHtml . contents $ expression'

  post root $ do
      contents <- param' "Mathematical Expression"
      expressionRef <- expression <$> getState
      let splitInfixExpr = combineNum . splitToList . concat . removeItem "\r" . removeItem "\n" . combineNum . removePlusNum . addZeroExponent . addZeroStringUnaryHeadPositiveOrNegative . removeUnaryHeadPositive . combineUnaryOperators . splitToList . unpack $ contents
      let isInfixValid = infixValidator $ splitInfixExpr
      let infixCalculation = evaluatePostfix . infixToPostfix $ splitInfixExpr
      if isInfixValid
          then do
                -- only will ever be Just x
              case infixCalculation of
                  (Just x)  ->
                      liftIO $ atomicModifyIORef' expressionRef $ \expression ->
                          (Expression (pack $ ("Calculated Answer: "++printf "%.3f" x)), ())
          else liftIO $ atomicModifyIORef' expressionRef $ \expression ->
              (Expression "Answer could not be calculated", ())
      redirect "/" -- refresh screen

main :: IO ()
main = do
    let expr = Expression ""
    st <- ServerState <$> newIORef expr-- []
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)
