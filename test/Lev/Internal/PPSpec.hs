module Lev.Internal.PPSpec (spec) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Lev.Internal
import Prettyprinter
import Test.Hspec
import Text.Trifecta
import Prelude hiding (pi)

spec :: Spec
spec = do
  parseExprSpec

parseExprSpec :: Spec
parseExprSpec = describe "exprP" $ do
  let vx, vy :: Term T.Text
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
  "data"
    ~~~ "(data desc param)"
    <==> Data "desc" "param"

parsesAs :: (HasCallStack) => T.Text -> Term T.Text -> Expectation
parsesAs s x = case parseString exprP mempty (T.unpack s) of
  Success y -> y `shouldBe` x
  Failure e -> expectationFailure $ show e

printsAs :: (HasCallStack) => Term T.Text -> T.Text -> Expectation
printsAs parsed textual =
  renderStrict (layoutPretty defaultLayoutOptions $ pretty parsed)
    `shouldBe` textual

infixr 1 ~~~

(~~~) :: (HasCallStack) => String -> (T.Text, Term T.Text) -> Spec
msg ~~~ (textual, parsed) =
  context msg $ do
    it "pretty-prints correctly" $ parsed `printsAs` textual
    it "parses correctly" $ textual `parsesAs` parsed

infixr 2 <==>

(<==>) :: (HasCallStack) => a -> b -> (a, b)
(<==>) = (,)
