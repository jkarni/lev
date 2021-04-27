{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Lev.Internal.ExprSpec
  ( spec,
  )
where

import Data.Either
import Lev.Internal.Expr
import Test.Hspec
import Prelude hiding (exp, pi)

spec :: Spec
spec = do
  inferTypeSpec
  checkTypeSpec

inferTypeSpec :: Spec
inferTypeSpec = describe "inferType" $ do
  let infers :: Term () String -> Term () String -> Expectation
      infers x y = runM (inferType emptyCtx x) `shouldBe` Right y
      fails :: Term () String -> Expectation
      fails x = runM (inferType emptyCtx x) `shouldSatisfy` isLeft
  it "infers the type of a correctly annotated term" $ do
    infers (Type () -: Type ()) (Type ())
  it "rejects incorrectly annotated terms" $ do
    fails ((UnitType ()) -: (UnitType ()))
    fails (TagType () -: Tag () "x")
    fails ((UnitType ()) -: (UnitValue ()))
  it "infers the type of an application" $ do
    let fn = lambda () "x" (Var () "x")
        typ = pi "t" (Type ()) (Var () "t")
        v = Type ()
    infers ((fn -: typ) $$ v) (Type ())
  it "rejects ill-typed applications" $ do
    fails ((Type () -: Type ()) $$ (Type ()))
  it "infers the type of ()" $ do
    infers (UnitValue ()) (UnitType ())
  it "infers the type of Unit" $ do
    infers (UnitType ()) (Type ())
  it "infers the type of a tag" $ do
    infers (Tag () "aTag") (TagType ())
  it "infers the type of Tag" $ do
    infers (TagType ()) (Type ())
  it "infers the type of Unquote ()" $ do
    let fn = lambda () "x" (Var () "x")
        typ = pi "t" (Type ()) (Var () "t")
        v = (Type ())
    infers (Unquote () (Pair () (Type ()) (UnitValue ()))) (Type ())
    infers (Unquote () (Pair () (fn -: typ) v)) (Type ())

checkTypeSpec :: Spec
checkTypeSpec = describe "checkType" $ do
  let checks :: (HasCallStack) => Term () String -> Term () String -> Expectation
      checks val typ = runM (checkType emptyCtx val typ) `shouldBe` Right ()
      doesn'tCheck :: Term () String -> Term () String -> Expectation
      doesn'tCheck val typ = runM (checkType emptyCtx val typ) `shouldSatisfy` isLeft
  it "accepts correctly typed pi terms" $ do
    checks (lambda () "x" (Var () "x")) (fnType (UnitType ()) (UnitType ()))
  it "rejects incorrectly typed pi terms" $ do
    doesn'tCheck (lambda () "x" (Var () "x")) (Type ())
    doesn'tCheck (UnitValue ()) (pi "t" (Type ()) (Var () "t"))
  it "accepts correctly typed sigma terms" $ do
    checks (pair (Type ()) (Type ())) (sigma () "t" (Type ()) (Var () "t"))
  it "rejects incorrectly typed sigma terms" $ do
    doesn'tCheck (pair (Type ()) (UnitValue ())) (sigma () "t" (Type ()) (Var () "t"))
  it "accepts correctly typed EndDesc ()s" $ do
    checks (EndDesc () (UnitType ())) (Description () (Type ()))
  it "rejects incorrectly typed EndDesc ()s" $ do
    doesn'tCheck (EndDesc () (UnitValue ())) (Description () (Type ()))
  it "accepts correctly typed RecDesc ()s" $ do
    checks (RecDesc () (UnitValue ()) (EndDesc () (UnitValue ()))) (Description () (UnitType ()))
  it "rejects incorrectly typed RecDesc ()s" $ do
    doesn'tCheck (RecDesc () (UnitValue ()) (EndDesc () (UnitType ()))) (Description () (UnitType ()))
    doesn'tCheck (RecDesc () (UnitType ()) (EndDesc () (UnitType ()))) (Description () (UnitType ()))
  it "accepts correctly typed ArgDesc ()s" $ do
    checks (RecDesc () (UnitValue ()) (EndDesc () (UnitValue ()))) (Description () (UnitType ()))
  it "rejects incorrectly typed ArgDesc ()s" $ do
    let fn =
          lambda () "x" (EndDesc () (Var () "x"))
            -: fnType (UnitType ()) (Description () (UnitType ()))
    checks (ArgDesc () (UnitType ()) fn) (Description () (UnitType ()))
