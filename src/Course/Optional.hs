{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import Course.Core
import qualified Prelude as P

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a = Full a | Empty deriving (Eq, Show)

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty    = Empty
mapOptional f (Full a) = Full (f a)

bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty    = Empty
bindOptional f (Full a) = f a

(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

-- Note the infix definitions. Looks like an add operation that
-- selects the first non-empty value.
(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

-- Takes a binary function between two unwrapped values, then combines
-- the two optionals using that binary operation. If either is an
-- Empty, the final result is an empty.
--
-- (f aa) curries the first arg of the binary function, and the
-- monadic function internally maps it across the second Optional
-- value. The bind handles getting that first value into the mix.
twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f a b = bindOptional (\aa -> mapOptional (f aa) b) a

-- |Is the wrapped value equal to the supplied unwrapped value?
contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Monad Optional where
  (>>=)  = flip bindOptional
  return = Full
