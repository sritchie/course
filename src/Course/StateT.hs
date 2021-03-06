{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT { runStateT :: s -> f (a, s) }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
--
-- This bad boy's job is to use the functor to map the incoming
-- function against the value piece of the StateT result.
instance Functor f => Functor (StateT s f) where
  f <$> StateT k = StateT ((first f <$>) . k)

-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
instance Bind f => Apply (StateT s f) where
  StateT l <*> StateT r =
    StateT (\s -> do (f, s') <- l s; first f <$> r s')

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
instance Monad f => Applicative (StateT s f) where
  pure x = StateT (\s -> pure (x,s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
instance Monad f => Bind (StateT s f) where
  f =<< StateT k =
    -- StateT (\s -> k s >>= (\(a, s') -> runStateT (f a) s'))
    StateT ((=<<) (\(a, s') -> runStateT (f a) s') . k)

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a = StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' :: (s -> (a, s)) -> State' s a
state' f = StateT (pure . f)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' (StateT k) = runId . k

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: Functor f => StateT s f a -> s -> f s
execT (StateT k) = (<$>) snd . k

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: State' s a -> s -> s
exec' t = runId . execT t

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: Functor f => StateT s f a -> s -> f a
evalT (StateT k) = (<$>) fst . k

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' :: State' s a -> s -> a
eval' t = runId . evalT t

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Monad f => StateT s f s
getT = StateT (\s -> pure (s,s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Monad f => s -> StateT s f ()
putT = StateT . const . pure . ((),)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
distinct' :: (Ord a, Num a) => List a -> List a
distinct' xs = let efn a = state' (S.notMember a &&& S.insert a)
               in eval' (filtering efn xs) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF xs =
  let efn a = StateT
              (\s ->
                if a > 100
                then Empty
                else Full (S.notMember a s, S.insert a s))
  in evalT (filtering efn xs) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT { runOptionalT :: f (Optional a) }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> OptionalT k = OptionalT ((f <$>) <$> k)

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
--  Yet another one that took a LONG time to write.
instance Apply f => Apply (OptionalT f) where
  OptionalT f <*> OptionalT k =
    OptionalT (lift2 (<*>) f k)

    -- k is `f Optional a`, f is `f Optional (a -> b)`
    -- need f (Optional a -> Optional b)
    -- Optional (a -> b) -> Optional a -> Optional b

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure = OptionalT . pure . pure

-- | Implement the `Bind` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Bind (OptionalT f) where
  f =<< OptionalT k =
    OptionalT ((\opta -> case opta of
                 Empty -> pure Empty
                 Full a -> runOptionalT (f a))
               =<< k)

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> Logger l v = Logger l (f v)

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  Logger l f <*> Logger r a =
    Logger (l ++ r) (f a)

-- | Implement the `Applicative` instance for `Logger`.
instance Applicative (Logger l) where
  pure = Logger Nil

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Bind (Logger l) where
  f =<< Logger l a =
    let Logger l' b = f a
    in  Logger (l ++ l') b

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG xs =
  let stringcat x y = fromString (x P.++ show y)
      efn a = StateT
              (OptionalT .
               (\ret ->
                 if a > 100
                 then log1 (stringcat "aborting > 100: " a) Empty
                 else (if even a
                       then log1 (stringcat "even: " a)
                       else pure) (Full ret)) .
               (S.notMember a &&& S.insert a))
  in runOptionalT (evalT (filtering efn xs) S.empty)
