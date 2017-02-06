module Lev.Internal.ExprSpec (spec) where

import Prelude hiding (exp, pi)
import Test.Hspec
import Lev.Internal.Expr

spec :: Spec
spec = do
  inferTypeSpec
  checkTypeSpec

inferTypeSpec :: Spec
inferTypeSpec = describe "inferType" $ do
  let infers :: Term String -> Term String -> Expectation
      infers x y = inferType emptyCtx x `shouldBe` Right y
      {-fails  x   = inferrable-}

  it "infers the type of a correctly annotated term" $ do
    infers (Annotation Type Type) Type

  it "rejects incorrectly annotated terms" $ pending

  it "infers the type of an application" $ do
    let fn = lambda "x" (Var "x")
        typ = pi "t" Type (Var "t")
        v  = Type
    infers (Application (Annotation fn typ) v) Type

  it "rejects ill-typed applications" $ pending

checkTypeSpec :: Spec
checkTypeSpec = describe "checkType" $ do

  it "accepts correctly typed pi terms" $ do
    let r = checkType emptyCtx (lambda "x" (Var "x")) (pi "t" Type (Var "t"))
    r `shouldBe` Right ()
