# purescript-pointed-list

Port of Haskell pointedlist library.

## Type

Single type provided by this library is `Data.List.Pointed`:

```purescript
newtype Pointed a = Pointed
  { focus ∷ a
  , reversedPrefix ∷ List a
  , suffix ∷ List a
  }
```

It allows to quickly move focus and update or replace value at the current position.

## Operations

Currently this library provides bunch of instances: `Foldable`, `Foldable1`, `Traversable`, `Functor`, `Comonad`. If you want to extend this set please don't hesitate to open a PR.

Additionally it implements following operations:

* `prev`, `next`, `insertLeft`, `insertRight`, `deleteLeft`, `deleteRight`, `replace`, `dropPrefix`, `dropSuffix`, `atStart`, `atEnd` - O(1)
* `first`, `last` - O(n)

## Usage example

`Pointed` list structure can be useful for example when you are implementing history browsing with `undo / redo` operations.
