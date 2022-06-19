module Data.List.Pointed where

import Prelude
import Control.Comonad (class Comonad, class Extend)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.List (List(..), reverse, uncons)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Semigroup.Foldable (class Foldable1, foldl1Default, foldr1Default)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Partial.Unsafe (unsafePartial)

newtype Pointed a = Pointed
  { focus :: a
  , reversedPrefix :: List a
  , suffix :: List a
  }

derive instance Functor Pointed

derive instance Newtype (Pointed a) _

instance Extend Pointed where
  extend f initial@(Pointed { reversedPrefix, suffix }) = Pointed { focus: f initial, reversedPrefix: reversedPrefix', suffix: suffix' }
    where
    step move (p@{ prevPointed, accum }) _ = case move prevPointed of
      Just newPointed -> { prevPointed: newPointed, accum: accum <<< Cons (f newPointed) }
      Nothing -> p

    suffix' = (_.accum <<< foldl (step next) { prevPointed: initial, accum: identity } $ suffix) $ Nil

    reversedPrefix' = (_.accum <<< foldl (step prev) { prevPointed: initial, accum: identity } $ reversedPrefix) $ Nil

instance Comonad Pointed where
  extract (Pointed { focus }) = focus

instance Foldable Pointed where
  foldr f z (Pointed { focus, reversedPrefix, suffix }) = foldl (flip f) (f focus (foldr f z suffix)) reversedPrefix
  foldl f z (Pointed { focus, reversedPrefix, suffix }) = foldl f (f (foldr (flip f) z reversedPrefix) focus) suffix
  foldMap f (Pointed { focus, reversedPrefix, suffix }) = foldMap f (reverse reversedPrefix) <> f focus <> foldMap f suffix

instance Traversable Pointed where
  sequence (Pointed { focus, reversedPrefix, suffix }) =
    Pointed
      <$> ({ reversedPrefix: _, focus: _, suffix: _ } <$> sequence (reverse reversedPrefix) <*> focus <*> sequence suffix)
  traverse = traverseDefault

instance Foldable1 Pointed where
  foldr1 f = foldr1Default f
  foldl1 f = foldl1Default f
  foldMap1 f (Pointed { suffix, reversedPrefix, focus }) = foldl (\r e -> r <> f e) (foldl (\r e -> f e <> r) (f focus) reversedPrefix) suffix

instance Eq a => Eq (Pointed a) where
  eq (Pointed { suffix: a1, reversedPrefix: b1, focus: f1 }) (Pointed { suffix: a2, reversedPrefix: b2, focus: f2 }) = a1 == a2 && b1 == b2 && f1 == f2

instance Eq1 Pointed where
  eq1 = eq

-- | Build and set focus at the end.
fromFoldable :: forall a f. Foldable f => f a -> Maybe (Pointed a)
fromFoldable f = do
  let
    revAll = foldl (flip Cons) Nil f
  { head, tail } <- uncons revAll
  pure $ Pointed { focus: head, reversedPrefix: tail, suffix: Nil }

fromFoldable1 :: forall a f. Foldable1 f => f a -> Pointed a
fromFoldable1 f = unsafePartial (fromJust $ fromFoldable $ f)

singleton :: forall a. a -> Pointed a
singleton a = Pointed { focus: a, reversedPrefix: Nil, suffix: Nil }

next :: forall a. Pointed a -> Maybe (Pointed a)
next (Pointed { focus, reversedPrefix, suffix }) =
  uncons suffix
    <#> \{ head, tail } ->
      Pointed
        { focus: head
        , reversedPrefix: Cons focus reversedPrefix
        , suffix: tail
        }

prev :: forall a. Pointed a -> Maybe (Pointed a)
prev (Pointed { focus, reversedPrefix, suffix }) =
  uncons reversedPrefix
    <#> \{ head, tail } ->
      Pointed
        { focus: head
        , reversedPrefix: tail
        , suffix: Cons focus suffix
        }

replace :: forall a. a -> Pointed a -> Pointed a
replace a (Pointed { reversedPrefix, suffix }) = Pointed { focus: a, reversedPrefix, suffix }

-- | Insert element before current focus and move
-- | focus to the new one.
insertLeft :: forall a. a -> Pointed a -> Pointed a
insertLeft a (Pointed { focus, reversedPrefix, suffix }) =
  Pointed
    { focus: a
    , reversedPrefix
    , suffix: Cons focus suffix
    }

insertRight :: forall a. a -> Pointed a -> Pointed a
insertRight a (Pointed { focus, reversedPrefix, suffix }) =
  Pointed
    { focus: a
    , reversedPrefix: Cons focus reversedPrefix
    , suffix
    }

-- | Delete current `focus` and move to the left if possible.
deleteLeft :: forall a. Pointed a -> Maybe (Pointed a)
deleteLeft (Pointed { reversedPrefix, suffix }) =
  uncons reversedPrefix
    <#> \{ head, tail } ->
      Pointed
        { focus: head, reversedPrefix: tail, suffix }

-- | Delete current `focus` and move to the right if possible.
deleteRight :: forall a. Pointed a -> Maybe (Pointed a)
deleteRight (Pointed { reversedPrefix, suffix }) =
  uncons suffix
    <#> \{ head, tail } ->
      Pointed
        { focus: head, reversedPrefix, suffix: tail }

moveLeft :: forall a. Pointed a -> Maybe (Pointed a)
moveLeft (Pointed { focus, reversedPrefix, suffix }) =
  uncons reversedPrefix
    <#> \{ head, tail } ->
      Pointed
        { focus, reversedPrefix: tail, suffix: Cons head suffix }

moveRight :: forall a. Pointed a -> Maybe (Pointed a)
moveRight (Pointed { focus, reversedPrefix, suffix }) =
  uncons suffix
    <#> \{ head, tail } ->
      Pointed
        { focus, reversedPrefix: Cons head reversedPrefix, suffix: tail }

dropPrefix :: forall a. Pointed a -> Pointed a
dropPrefix (Pointed { focus, suffix }) = Pointed { focus, reversedPrefix: Nil, suffix }

dropSuffix :: forall a. Pointed a -> Pointed a
dropSuffix (Pointed { focus, reversedPrefix }) = Pointed { focus, reversedPrefix, suffix: Nil }

-- | Change `focus` so it points at first element.
first :: forall a. Pointed a -> Pointed a
first p = case prev p of
  Just p' -> first p'
  Nothing -> p

-- | Change `focus` so it points at last element.
last :: forall a. Pointed a -> Pointed a
last p = case next p of
  Just p' -> last p'
  Nothing -> p

atStart :: forall a. Pointed a -> Boolean
atStart (Pointed { reversedPrefix: Nil }) = true

atStart _ = false

atEnd :: forall a. Pointed a -> Boolean
atEnd (Pointed { suffix: Nil }) = true

atEnd _ = false
