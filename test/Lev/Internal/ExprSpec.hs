{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
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

  let infers :: Term Int -> Term Int -> Expectation
      infers x y = inferType emptyCtx x `shouldBe` Right y
      fails  :: Term Int -> Expectation
      fails  x   = inferType emptyCtx x `shouldSatisfy` isLeft

  it "infers the type of a correctly annotated term" $ do
    infers (Type -: Type) Type

  it "rejects incorrectly annotated terms" $ do
    fails (UnitType -: UnitType)
    fails (TagType -: Tag "x")
    fails (UnitType -: UnitValue)

  it "infers the type of an application" $ do
    let fn  = lambda 0 (Var 0)
        typ = pi 1 Type (Var 1)
        v   = Type
    infers ((fn -: typ) $$ v) Type

  it "rejects ill-typed applications" $ do
    fails ((Type -: Type) $$ Type)

  it "infers the type of ()" $ do
    infers UnitValue UnitType

  it "infers the type of Unit" $ do
    infers UnitType Type

  it "infers the type of a tag" $ do
    infers (Tag "aTag") TagType

  it "infers the type of Tag" $ do
    infers TagType Type

checkTypeSpec :: Spec
checkTypeSpec = describe "checkType" $ do

  let checks :: Term Int -> Term Int -> Expectation
      checks val typ = checkType emptyCtx val typ `shouldBe` Right ()
      doesn'tCheck :: Term Int -> Term Int -> Expectation
      doesn'tCheck val typ = checkType emptyCtx val typ `shouldSatisfy` isLeft

  it "accepts correctly typed pi terms" $ do
    {-checks (lambda "x" (Var "x")) (fnType UnitType UnitType)-}
    checks (lambda 0 (Var 0)) (pi 1 UnitType UnitType)

  it "rejects incorrectly typed pi terms" $ do
    doesn'tCheck (lambda 0 (Var 0)) Type
    doesn'tCheck UnitValue (pi 0 Type (Var 0))

  {-it "accepts correctly typed sigma terms" $ do-}
    {-checks (pair Type Type) (sigma "t" Type (Var "t"))-}

  {-it "rejects incorrectly typed sigma terms" $ do-}
    {-doesn'tCheck (pair Type UnitValue) (sigma "t" Type (Var "t"))-}

  {-it "accepts correctly typed EndDescs" $ do-}
    {-checks (EndDesc UnitType) (Description Type)-}

  {-it "rejects incorrectly typed EndDescs" $ do-}
    {-doesn'tCheck (EndDesc UnitValue) (Description Type)-}

  {-it "accepts correctly typed RecDescs" $ do-}
    {-checks (RecDesc UnitValue (EndDesc UnitValue)) (Description UnitType)-}

  {-it "rejects incorrectly typed RecDescs" $ do-}
    {-doesn'tCheck (RecDesc UnitValue (EndDesc UnitType)) (Description UnitType)-}
    {-doesn'tCheck (RecDesc UnitType (EndDesc UnitType)) (Description UnitType)-}

  {-it "accepts correctly typed ArgDescs" $ do-}
    {-checks (RecDesc UnitValue (EndDesc UnitValue)) (Description UnitType)-}

  {-it "rejects incorrectly typed ArgDescs" $ do-}
    {-let fn = lambda "x" (EndDesc (Var "x"))-}
          {--: fnType UnitType (Description UnitType)-}
    {-checks (ArgDesc UnitType fn) (Description UnitType)-}
