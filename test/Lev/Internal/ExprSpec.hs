module Lev.Internal.ExprSpec (spec) where

import Prelude hiding (exp, pi)

import Data.Either
import Lev.Internal.Expr
import Test.Hspec

spec :: Spec
spec = do
  inferTypeSpec
  checkTypeSpec

inferTypeSpec :: Spec
inferTypeSpec = describe "inferType" $ do

  let infers :: Term String -> Term String -> Expectation
      infers x y = inferType emptyCtx x `shouldBe` Right y
      fails  :: Term String -> Expectation
      fails  x   = inferType emptyCtx x `shouldSatisfy` isLeft

  it "infers the type of a correctly annotated term" $ do
    infers (Type -: Type) Type

  it "rejects incorrectly annotated terms" $ pending

  it "infers the type of an application" $ do
    let fn  = lambda "x" (Var "x")
        typ = pi "t" Type (Var "t")
        v   = Type
    infers ((fn -: typ) $$ v) Type

  it "rejects ill-typed applications" $ do
    fails ((Type -: Type) $$ Type)

checkTypeSpec :: Spec
checkTypeSpec = describe "checkType" $ do

  it "accepts correctly typed pi terms" $ do
    let r = checkType emptyCtx (lambda "x" (Var "x")) (pi "t" Type (Var "t"))
    r `shouldBe` Right ()

  it "rejects incorrectly typed pi terms" $ do
    let r = checkType emptyCtx (lambda "x" (Var "x")) Type
    r `shouldSatisfy` isLeft
