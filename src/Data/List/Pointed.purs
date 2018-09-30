module Data.List.Pointed where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.List (List(..), reverse, uncons)
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Traversable (class Traversable, sequence, traverseDefault)

newtype Pointed a = Pointed
  { focus ∷ a
  , reversedPrefix ∷ List a
  , suffix ∷ List a
  }
derive instance functorPointed ∷ Functor Pointed
derive instance newtypePointed ∷ Newtype (Pointed a) _

instance extendPointed ∷ Extend Pointed where
  extend f initial@(Pointed { focus, reversedPrefix, suffix }) =
    Pointed { focus: f initial, reversedPrefix: reversedPrefix', suffix: suffix' }
    where
    step move (p@{ prevPointed, accum }) _ = case move prevPointed of
      Just newPointed →
        { prevPointed: newPointed, accum: accum <<< Cons (f newPointed) }
      Nothing → p
    suffix' =
      (_.accum <<< foldl (step next) { prevPointed: initial, accum: identity } $ suffix) $ Nil
    reversedPrefix' =
      (_.accum <<< foldl (step prev) { prevPointed: initial, accum: identity } $ reversedPrefix) $ Nil

instance extractPointed ∷ Comonad Pointed where
  extract (Pointed { focus }) = focus

instance foldablePointed ∷ Foldable Pointed where
  foldr f z (Pointed { focus, reversedPrefix, suffix }) =
    foldr f (f focus (foldr f z suffix)) (reverse reversedPrefix)

  foldl f z (Pointed { focus, reversedPrefix, suffix }) =
    foldl f (f (foldr (flip f) z (reversedPrefix)) focus) suffix

  foldMap f (Pointed { focus, reversedPrefix, suffix }) =
    foldMap f (reverse reversedPrefix) <> f focus <> foldMap f suffix

instance traversablePointed ∷ Traversable Pointed where
  sequence (Pointed { focus, reversedPrefix, suffix }) = Pointed <$>
    ({ reversedPrefix: _, focus: _, suffix: _ } <$> sequence (reverse reversedPrefix) <*> focus <*> sequence suffix)
  traverse = traverseDefault

instance foldable1Pointed ∷ Foldable1 Pointed where
  fold1 (Pointed { suffix, reversedPrefix, focus }) =
    foldl (<>) (foldl (flip (<>)) focus reversedPrefix) suffix

  foldMap1 f (Pointed { suffix, reversedPrefix, focus }) =
    foldl (\r e → r <> f e) (foldl (\r e → f e <> r) (f focus) reversedPrefix) suffix

instance eqPointed ∷ Eq a ⇒ Eq (Pointed a) where
  eq
    (Pointed { suffix: a1, reversedPrefix: b1, focus: f1 })
    (Pointed { suffix: a2, reversedPrefix: b2, focus: f2 }) =
      a1 == a2 && b1 == b2 && f1 == f2

-- | Build and set focus at the end.
fromFoldable ∷ ∀ a f. Foldable f ⇒ f a → Maybe (Pointed a)
fromFoldable f = do
  let revAll = (reverse $ List.fromFoldable f)
  { head, tail } ← uncons revAll
  pure $ Pointed { focus: head, reversedPrefix: tail, suffix: Nil }

singleton ∷ ∀ a. a → Pointed a
singleton a = Pointed { focus: a, reversedPrefix: Nil, suffix: Nil }

next ∷ ∀ a. Pointed a → Maybe (Pointed a)
next (Pointed { focus, reversedPrefix, suffix }) =
  uncons suffix <#> \{ head, tail } → Pointed
    { focus: head
    , reversedPrefix: Cons focus reversedPrefix
    , suffix: tail
    }

prev ∷ ∀ a. Pointed a → Maybe (Pointed a)
prev (Pointed { focus, reversedPrefix, suffix }) =
  uncons reversedPrefix <#> \{ head, tail } → Pointed
    { focus: head
    , reversedPrefix: tail
    , suffix: Cons focus suffix
    }

replace ∷ ∀ a. a → Pointed a → Pointed a
replace a (Pointed { reversedPrefix, suffix }) =
  Pointed { focus: a, reversedPrefix, suffix }

-- | Insert element before current focus and move
-- | focus to the new one.
insertLeft ∷ ∀ a. a → Pointed a → Pointed a
insertLeft a (Pointed { focus, reversedPrefix, suffix }) = Pointed
  { focus: a
  , reversedPrefix
  , suffix: Cons focus suffix
  }

insertRight ∷ ∀ a. a → Pointed a → Pointed a
insertRight a (Pointed { focus, reversedPrefix, suffix }) = Pointed
  { focus: a
  , reversedPrefix: Cons focus reversedPrefix
  , suffix
  }

deleteLeft ∷ ∀ a. Pointed a → Maybe (Pointed a)
deleteLeft (Pointed { focus, reversedPrefix, suffix }) =
  uncons reversedPrefix <#> \{ head, tail } → Pointed
    { focus: head, reversedPrefix: tail, suffix }

deleteRight ∷ ∀ a. Pointed a → Maybe (Pointed a)
deleteRight (Pointed { focus, reversedPrefix, suffix }) =
  uncons suffix <#> \{ head, tail } → Pointed
    { focus: head, reversedPrefix, suffix: tail }

dropPrefix ∷ ∀ a. Pointed a → Pointed a
dropPrefix (Pointed { focus, suffix }) =
  Pointed { focus, reversedPrefix: Nil, suffix }

dropSuffix ∷ ∀ a. Pointed a → Pointed a
dropSuffix (Pointed { focus, reversedPrefix }) =
  Pointed { focus, reversedPrefix, suffix: Nil }

atStart ∷ ∀ a. Pointed a → Boolean
atStart (Pointed { reversedPrefix: Nil }) = true
atStart _ = false

atEnd ∷ ∀ a. Pointed a → Boolean
atEnd (Pointed { suffix: Nil }) = true
atEnd _ = false

