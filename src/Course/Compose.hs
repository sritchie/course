{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Comonad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  f <$> Compose k = Compose $ (f <$>) <$> k

-- This one was rough too. Lots of type fuckery.
-- Also I need to get better at recognizing lift2 calls.
instance (Apply f, Apply g) => Apply (Compose f g) where
  -- Implement the (<*>) function for an Apply instance for Compose
  Compose f <*> Compose k = Compose $ lift2 (<*>) f k

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- Implement the pure function for an Applicative instance for Compose
    pure = Compose . pure . pure

-- I could only implement this by pulling in Comonad.
instance (Bind f, Bind g, Comonad f, Comonad g) => Bind (Compose f g) where
    -- Implement the (=<<) function for a Bind instance for Compose
  f =<< Compose k = copure (copure ((f <$>) <$> k))
