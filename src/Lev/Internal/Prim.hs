module Lev.Internal.Prim where

import qualified Data.Text as T
import Lev.Internal.Expr

prims :: [(T.Text, Term b a -> Term b a)]
prims = []
