{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lev.Internal.Pretty where

import Bound
import qualified Bound.Name as BN
import qualified Data.Text as T
import Lev.Internal.Expr
import Prettyprinter as P
import Prelude hiding (exp)

instance P.Pretty a => P.Pretty (Term a) where
  pretty = pretty'

pretty' :: P.Pretty a => Term a -> Doc ann
pretty' x = case x of
  Annotation a b -> P.parens $ ":" <+> pretty a <+> pretty b
  Application f v -> P.parens $ pretty f <+> pretty v
  Var v -> pretty v
  UnitValue -> "()"
  UnitType -> "Unit"
  Type -> "Type"
  Pi t var exp ->
    P.parens $ "pi" <+> pretty t <+> pretty var <+> pretty (fromScope exp)
  Tag t -> "#" <> pretty t
  TagType -> "Tag"
  Pair a b -> "'" <> P.parens (pretty a <+> pretty b)
  Lambda var exp ->
    P.parens $ "lam" <+> pretty var <+> pretty (fromScope exp)
  Description param -> P.parens $ "description" <+> pretty param
  EndDesc param -> P.parens $ "description-end" <+> pretty param
  RecDesc param fn ->
    P.parens $
      "description-rec" <+> pretty param
        <+> pretty fn
  ArgDesc param fn ->
    P.parens $
      "description-arg" <+> pretty param
        <+> pretty fn

instance (Pretty b, Pretty a) => Pretty (Var b a) where
  pretty x = case x of
    B b -> pretty b
    F y -> pretty y

instance P.Pretty (BN.Name T.Text One) where
  pretty (BN.Name var One) = pretty var
