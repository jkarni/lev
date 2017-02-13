module Lev.Internal.ParserSpec (spec) where

import Prelude hiding (pi)
import Test.Hspec
import Text.Trifecta
import Text.Trifecta.Result (Result(Success))

import qualified Data.Text as T

import Lev.Internal.Parser
import Lev.Internal.Expr


spec :: Spec
spec = do
  parseExprSpec

parseExprSpec :: Spec
parseExprSpec = describe "exprP" $ do

  let parsesAs :: String -> Term T.Text -> Expectation
      parsesAs s x = case parseString exprP mempty s of
       Success y -> y `shouldBe` x
       Failure e -> expectationFailure $ show e
      vx, vy :: Term T.Text
      vx = Var "x"
      vy = Var "y"

  it "parses variables" $ "x" `parsesAs` vx

  it "parses annotations" $ "(: x y)" `parsesAs` Annotation vx vy

  it "parses Unit" $ "()" `parsesAs` UnitValue

  it "parses UnitType" $ "Unit" `parsesAs` UnitType

  it "parses Type" $ "Type" `parsesAs` Type

  it "parses Pi" $ "(pi Type x y)" `parsesAs` pi "x" Type vy

  it "parses lambdas" $ "(lam x x)" `parsesAs` lambda "x" vx

  it "parses singletons" $ "'(x)" `parsesAs` Pair vx UnitValue

  it "parses pairs" $ "'(x y)" `parsesAs` Pair vx (Pair vy UnitValue)

  it "parses tuples" $ "'(x y x)" `parsesAs` Pair vx (Pair vy (Pair vx UnitValue))
