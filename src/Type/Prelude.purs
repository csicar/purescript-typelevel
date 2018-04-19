module Type.Prelude
  ( module Type.Data.Boolean
  , module Type.Data.Ordering
  , module Type.Data.Symbol
  , module Type.Equality
  , module Type.Proxy
  , module Type.Row
  ) where

import Type.Data.Boolean (kind Boolean, True, False, BProxy(..), class IsBoolean, reflectBoolean, reifyBoolean)
import Type.Data.Ordering (kind Ordering, LT, EQ, GT, OProxy(..), class IsOrdering, reflectOrdering, reifyOrdering)
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol, class Compare, compare, class Append, append, class Cons, uncons)
import Type.Equality (class TypeEquals, from, to)
import Type.Row (class Lacks, class RowToList, class ListToRow, RProxy(..), RLProxy(..))

