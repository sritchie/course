{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Traversable where

import Course.Functor
import Course.Apply
import Course.Applicative
import Course.List

class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable List where
  traverse f = foldRight (\a acc -> lift2 (:.) (f a) acc) (pure Nil)
