module Lev.Internal.Pretty where

import Bound
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
  Pi t v -> P.parens $ "pi" <+> pretty t <+> pretty (fromScope v)
  Tag t -> "#" <> pretty t
  TagType -> "Tag"
  Pair a b -> "'" <> P.parens (pretty a <+> pretty b)
  Lambda v -> P.parens $ "lam" <+> pretty (fromScope v)

instance Pretty a => Pretty (Var () a) where
  pretty x = case x of
    B () -> P.parens mempty
    F y -> pretty y
