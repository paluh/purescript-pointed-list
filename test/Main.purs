module Test.Main where

import Prelude

import Control.Comonad (extend, extract)
import Data.Array (fromFoldable, singleton, snoc) as Array
import Data.Foldable (fold, foldMap, sum)
import Data.List (length) as List
import Data.List.Pointed (Pointed(..), fromFoldable, insertLeft, insertRight, prev)
import Data.List.Pointed (fromFoldable) as Pointed
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Semigroup.Foldable (fold1)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $ do
  suite "Data.List.Pointed" $ do
    test "fromFoldable" do
      let arr = [1,2,3]
      equal (Array.fromFoldable <$> Pointed.fromFoldable arr) (Just arr)
    test "foldMap" do
      let arr = [1, 2, 3]
      equal (foldMap Array.singleton <$> Pointed.fromFoldable arr) (Just arr)
    test "fold" do
      let arr = [Additive 1, Additive 2, Additive 3]
      equal (fold <$> Pointed.fromFoldable arr) (Just (Additive 6))
    test "insertRight" do
      let
        arr = [1, 2, 3]
        new = 4
        p = insertRight new <$> Pointed.fromFoldable arr
      equal (Array.fromFoldable <$> p) (Just (Array.snoc arr new))
      equal (extract <$> p) (Just new)
    test "insertLeft" do
      let
        arr = [1, 2, 3]
        new = 4
        p = insertLeft new <$> Pointed.fromFoldable arr
      equal (Array.fromFoldable <$> p) (Just [1,2,4,3])
      equal (extract <$> p) (Just new)
    test "extend extract from the end" do
      let
        arr = [1, 2, 3]
        initial = Pointed.fromFoldable arr
        p = extend extract <$> initial
      equal (Array.fromFoldable <$> initial) (Array.fromFoldable <$> p)
      equal (extract <$> initial) (extract <$> p)
    test "extend extract from the beginning" do
      let
        arr = [1, 2, 3]
        initial = Pointed.fromFoldable arr >>= prev >>= prev
        p = extend extract <$> initial
      equal (Array.fromFoldable <$> initial) (Array.fromFoldable <$> p)
      equal (extract <$> initial) (extract <$> p)
    test "extend assoc end" do
      let
        arr = [1, 2, 3, 4]
        pos (Pointed { reversedPrefix }) = List.length reversedPrefix
        sum' (Pointed { suffix }) = sum suffix
        initial = Pointed.fromFoldable arr
        p = extend sum' <<< extend sum' <$> initial
        p' =  extend (sum' <<< extend sum') <$> initial
      equal (Array.fromFoldable <$> p') (Array.fromFoldable <$> p)
      equal (extract <$> p') (extract <$> p)
    test "extend assoc" do
      let
        arr = [1, 2, 3, 4]
        pos (Pointed { reversedPrefix }) = List.length reversedPrefix
        sum' (Pointed { suffix }) = sum suffix
        initial = Pointed.fromFoldable arr >>= prev
        p = extend sum' <<< extend sum' <$> initial
        p' =  extend (sum' <<< extend sum') <$> initial
      equal (Array.fromFoldable <$> p') (Array.fromFoldable <$> p)
      equal (extract <$> p') (extract <$> p)
    test "extend assoc start" do
      let
        arr = [1, 2, 3, 4]
        pos (Pointed { reversedPrefix }) = List.length reversedPrefix
        sum' (Pointed { suffix }) = sum suffix
        initial = Pointed.fromFoldable arr >>= prev >>= prev
        p = extend sum' <<< extend sum' <$> initial
        p' =  extend (sum' <<< extend sum') <$> initial
      equal (Array.fromFoldable <$> p') (Array.fromFoldable <$> p)
      equal (extract <$> p') (extract <$> p)
    test "fold1 from end" do
      let r = map fold1 (fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "fold1" do
      let r = map fold1 (prev =<< prev =<< fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "fold1 from start" do
      let r = map fold1 (prev =<< prev =<< prev =<< prev =<< fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r

