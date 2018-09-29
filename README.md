# purescript-pointed-list

Port of Haskell pointedlist library.

## Type

This library provides this type:

```purescript
newtype Pointed a = Pointed
  { focus ∷ a
  , reversedPrefix ∷ List a
  , suffix ∷ List a
  }
```

It allows quickly move focus and update or replace value at the current position.

## Operations

Currently this library provides bunch of instances: `Foldable`, `Foldable1`, `Traversable`, `Functor`, `Comonad`. If you want to extend this set please don't hesitate to open a PR.

Additionally it implements following operations:

* `prev` / `next` - O(1)
* `insertLeft` / `insertRight` - O(1)
* `deleteLeft` / `deleteRight` - O(1)
* `replace` - O(1)
* `dropSuffix` - O(1)
* `dropPrefix` - O(1)
* `atEnd` - O(1)
* `atStart` - O(1)

## Usage example

`Pointed` list structure can be useful for example when you are implementing history browsing with `undo / redo` operations.
