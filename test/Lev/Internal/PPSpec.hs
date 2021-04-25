module Lev.Internal.PPSpec (spec) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Lev.Internal
import Prettyprinter
import Test.Hspec
import Text.Trifecta
import Text.Trifecta.Delta (Delta)
import Prelude hiding (pi)

spec :: Spec
spec = do
  parseExprSpec

parseExprSpec :: Spec
parseExprSpec = describe "exprP" $ do
  let vx, vy :: Term a T.Text
      vx = Var "x"
      vy = Var "y"
  "variables"
    ~~~ "x"
    <==> vx
  "annotations"
    ~~~ "(: x y)"
    <==> Annotation vx vy
  "Unit"
    ~~~ "()"
    <==> UnitValue
  "UnitType"
    ~~~ "Unit"
    <==> UnitType
  "Type"
    ~~~ "Type"
    <==> Type
  "Pi"
    ~~~ "(pi Type x y)"
    <==> pi "x" Type vy
  "lambdas"
    ~~~ "(lam x x)"
    <==> lambda "x" vx
  "singletons"
    ~~~ "'(x)"
    <==> Pair vx UnitValue
  "pairs"
    ~~~ "'(x y)"
    <==> Pair vx (Pair vy UnitValue)
  "tuples"
    ~~~ "'(x y x)"
    <==> Pair vx (Pair vy (Pair vx UnitValue))
  "applications"
    ~~~ "(fn x y)"
    <==> Application (Application (Var "fn") vx) vy
  "rec"
    ~~~ "(description-rec type param)"
    <==> RecDesc "type" "param"
  "arg"
    ~~~ "(description-arg type param)"
    <==> ArgDesc "type" "param"
  "end"
    ~~~ "(description-end type)"
    <==> EndDesc "type"
  "description"
    ~~~ "(description type)"
    <==> Description "type"

parsesAs :: (HasCallStack) => T.Text -> Term Delta T.Text -> Expectation
parsesAs s x = case parseString exprP mempty (T.unpack s) of
  Success y -> y `shouldBe` x
  Failure e -> expectationFailure $ show e

printsAs :: (HasCallStack) => Term a T.Text -> T.Text -> Expectation
printsAs parsed textual =
  renderStrict (layoutPretty defaultLayoutOptions $ pretty parsed)
    `shouldBe` textual

infixr 1 ~~~

(~~~) :: (HasCallStack) => String -> (T.Text, Term Delta T.Text) -> Spec
msg ~~~ (textual, parsed) =
  context msg $ do
    it "pretty-prints correctly" $ parsed `printsAs` textual
    it "parses correctly" $ textual `parsesAs` parsed

infixr 2 <==>

(<==>) :: (HasCallStack) => a -> b -> (a, b)
(<==>) = (,)
