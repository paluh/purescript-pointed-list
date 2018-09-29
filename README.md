# purescript-pointed-list

Port of Haskell pointedlist library.

## Characteristics

* `prev` / `next` - O(1)
* `suffix - O(1)
* `insertLeft` / `insertRight` - O(1)
* `deleteLeft` / `deleteRight` - O(1)
* `replace` - O(1)
* `dropSuffix` - O(1)
* `dropPrefix` - O(1)
* `atEnd` - O(1)
* `atStart` - O(1)

## Usage

This type can be useful for example when you are implementing history browsing with `undo / redo` operations. Appending to the history is constant in time.
