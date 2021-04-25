{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lev.Internal.Pretty where

import Bound
import qualified Bound.Name as BN
import qualified Data.Text as T
import Lev.Internal.Expr
import Prettyprinter as P
import Prelude hiding (exp)

instance P.Pretty a => P.Pretty (Term b a) where
  pretty = pretty'

pretty' :: P.Pretty a => Term b a -> Doc ann
pretty' x = case x of
  Annotation _ a b -> P.parens $ ":" <+> pretty a <+> pretty b
  Application _ f v -> P.parens $ pretty f <+> pretty v
  Var _ v -> pretty v
  UnitValue _ -> "()"
  UnitType _ -> "Unit"
  Type _ -> "Type"
  Pi _ t var exp ->
    P.parens $ "pi" <+> pretty t <+> pretty var <+> pretty (fromScope exp)
  Tag _ t -> "#" <> pretty t
  TagType _ -> "Tag"
  Pair _ a b -> "'" <> P.parens (pretty a <+> pretty b)
  Lambda _ var exp ->
    P.parens $ "lam" <+> pretty var <+> pretty (fromScope exp)
  Description _ param -> P.parens $ "description" <+> pretty param
  EndDesc _ param -> P.parens $ "description-end" <+> pretty param
  RecDesc _ param fn ->
    P.parens $
      "description-rec" <+> pretty param
        <+> pretty fn
  ArgDesc _ param fn ->
    P.parens $
      "description-arg" <+> pretty param
        <+> pretty fn

instance (Pretty b, Pretty a) => Pretty (Var b a) where
  pretty x = case x of
    B b -> pretty b
    F y -> pretty y

instance P.Pretty (BN.Name T.Text One) where
  pretty (BN.Name var One) = pretty var
