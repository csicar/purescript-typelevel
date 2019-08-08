module Type.Data.Ordering
  ( module Prim.Ordering
  , OProxy(..)
  , class IsOrdering
  , reflectOrdering
  , reifyOrdering
  , class Append
  , append
  , class Invert
  , invert
  , class Equals
  , class Compare
  , compare
  , class IsLt
  , isLt
  , class IsEq
  , isEq
  , class IsGt
  , isGt
  , class IsLte
  , isLte
  , class IsGte
  , isGte
  , equals
  ) where

import Data.Ordering (Ordering(..))
import Data.Symbol (SProxy)
import Prim.Ordering (kind Ordering, LT, EQ, GT)
import Prim.Symbol (class Compare) as Symbol
import Type.Data.Boolean (class Or, BProxy(..), False, True, kind Boolean)

-- | Value proxy for `Ordering` types
data OProxy (ordering :: Ordering) = OProxy

-- | Class for reflecting a type level `Ordering` at the value level
class IsOrdering (ordering :: Ordering) where
  reflectOrdering :: OProxy ordering -> Ordering

instance isOrderingLT :: IsOrdering LT where reflectOrdering _ = LT
instance isOrderingEQ :: IsOrdering EQ where reflectOrdering _ = EQ
instance isOrderingGT :: IsOrdering GT where reflectOrdering _ = GT

-- | Use a value level `Ordering` as a type-level `Ordering`
reifyOrdering :: forall r. Ordering -> (forall o. IsOrdering o => OProxy o -> r) -> r
reifyOrdering LT f = f (OProxy :: OProxy LT)
reifyOrdering EQ f = f (OProxy :: OProxy EQ)
reifyOrdering GT f = f (OProxy :: OProxy GT)

-- | Append two `Ordering` types together
-- | Reflective of the semigroup for value level `Ordering`
class Append (lhs :: Ordering)
             (rhs :: Ordering)
             (output :: Ordering) |
             lhs -> rhs output
instance appendOrderingLT :: Append LT rhs LT
instance appendOrderingEQ :: Append EQ rhs rhs
instance appendOrderingGT :: Append GT rhs GT

append :: forall l r o. Append l r o => OProxy l -> OProxy r -> OProxy o
append _ _ = OProxy

-- | Invert an `Ordering`
class Invert (ordering :: Ordering)
             (result :: Ordering) |
             ordering -> result
instance invertOrderingLT :: Invert LT GT
instance invertOrderingEQ :: Invert EQ EQ
instance invertOrderingGT :: Invert GT LT

invert :: forall i o. Invert i o => OProxy i -> OProxy o
invert _ = OProxy

class Equals (lhs :: Ordering)
             (rhs :: Ordering)
             (out :: Boolean) |
             lhs rhs -> out

instance equalsEQEQ :: Equals EQ EQ True
instance equalsLTLT :: Equals LT LT True
instance equalsGTGT :: Equals GT GT True
instance equalsEQLT :: Equals EQ LT False
instance equalsEQGT :: Equals EQ GT False
instance equalsLTEQ :: Equals LT EQ False
instance equalsLTGT :: Equals LT GT False
instance equalsGTLT :: Equals GT LT False
instance equalsGTEQ :: Equals GT EQ False

equals :: forall l r o. Equals l r o => OProxy l -> OProxy r -> BProxy o
equals _ _ = BProxy

-- | Compares type a b
class Compare a b (o :: Ordering) | a b -> o

compare :: forall a b o. Compare a b o => a -> b -> OProxy o
compare _ _ = OProxy

class IsLt a b (isLt :: Boolean) | a b -> isLt
instance isLtTrue ∷ (Compare a b o, Equals o LT isLt) => IsLt a b isLt

isLt :: forall a b isLt. IsLt a b isLt => a -> b -> BProxy isLt
isLt _ _ = BProxy

class IsGt a b (isGt :: Boolean) | a b -> isGt
instance isGtCompare :: (Compare a b o, Equals o GT isGt) => IsGt a b isGt

isGt :: forall a b isGt. IsGt a b isGt => a -> b -> BProxy isGt
isGt _ _ = BProxy

class IsEq a b (isEq :: Boolean) | a b -> isEq
instance isEqCompare :: (Compare a b o, Equals o EQ isEq) => IsEq a b isEq

isEq :: forall a b isEq. IsEq a b isEq => a -> b -> BProxy isEq
isEq _ _ = BProxy

class IsLte a b (isLte :: Boolean) | a b -> isLte
instance isLteFromIs ∷ (IsEq a b isEq, IsLt a b isLt, Or isEq isLt isLte) => IsLte a b isLte

isLte :: forall a b isLte. IsLte a b isLte => a -> b -> BProxy isLte
isLte _ _ = BProxy

class IsGte a b (isGte :: Boolean) | a b -> isGte
instance isGteFromIs :: (IsEq a b isEq, IsGt a b isGt, Or isEq isGt isGte) => IsGte a b isGte

isGte :: forall a b isGte. IsGte a b isGte => a -> b -> BProxy isGte
isGte _ _ = BProxy

instance compareOrd :: Symbol.Compare lhs rhs ord => Compare (SProxy lhs) (SProxy rhs) ord