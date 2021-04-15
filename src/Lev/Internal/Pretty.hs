{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lev.Internal.Pretty where

import Bound
import Bound.Scope (bindings)
import Lev.Internal.Expr
import Prettyprinter as P

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
  Pi t v ->
    let [var] = bindings v
     in P.parens $ "pi" <+> pretty t <+> pretty var <+> pretty (fromScope v)
  Tag t -> "#" <> pretty t
  TagType -> "Tag"
  Pair a b -> "'" <> P.parens (pretty a <+> pretty b)
  Lambda v ->
    let [var] = bindings v
     in P.parens $ "lam" <+> pretty var <+> pretty (fromScope v)
  Data tags desc -> P.parens $ "data" <+> pretty tags <+> pretty desc

instance (Pretty b, Pretty a) => Pretty (Var b a) where
  pretty x = case x of
    B b -> pretty b
    F y -> pretty y
