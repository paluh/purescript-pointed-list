module Test.Main where

import Prelude

import Control.Comonad (extend, extract)
import Data.Array (fromFoldable, snoc) as Array
import Data.Foldable (fold, foldl, foldMap, foldr, sum)
import Data.List.Pointed (Pointed(..), atEnd, atStart, first, insertLeft, insertRight, last, moveLeft, moveRight, prev)
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
      let arr = [1, 2, 3]
      equal (Array.fromFoldable <$> Pointed.fromFoldable arr) (Just arr)
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
    test "moveLeft" do
      let
        arr = [1, 2, 3]
        p = moveLeft <=< Pointed.fromFoldable $ arr
      equal (Array.fromFoldable <$> p) (Just [1,3,2])
      equal (extract <$> p) (Just 3)
    test "moveRight" do
      let
        arr = [1, 2, 3]
        p = moveRight <=< prev <=< prev <=< Pointed.fromFoldable $ arr
      equal (Array.fromFoldable <$> p) (Just [2,1,3])
      equal (extract <$> p) (Just 1)
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
        sum' (Pointed { suffix }) = sum suffix
        initial = Pointed.fromFoldable arr
        p = extend sum' <<< extend sum' <$> initial
        p' =  extend (sum' <<< extend sum') <$> initial
      equal (Array.fromFoldable <$> p') (Array.fromFoldable <$> p)
      equal (extract <$> p') (extract <$> p)
    test "extend assoc" do
      let
        arr = [1, 2, 3, 4]
        sum' (Pointed { suffix }) = sum suffix
        initial = Pointed.fromFoldable arr >>= prev
        p = extend sum' <<< extend sum' <$> initial
        p' =  extend (sum' <<< extend sum') <$> initial
      equal (Array.fromFoldable <$> p') (Array.fromFoldable <$> p)
      equal (extract <$> p') (extract <$> p)
    test "extend assoc start" do
      let
        arr = [1, 2, 3, 4]
        sum' (Pointed { suffix }) = sum suffix
        initial = Pointed.fromFoldable arr >>= prev >>= prev
        p = extend sum' <<< extend sum' <$> initial
        p' =  extend (sum' <<< extend sum') <$> initial
      equal (Array.fromFoldable <$> p') (Array.fromFoldable <$> p)
      equal (extract <$> p') (extract <$> p)
    test "fold1 from end" do
      let r = map fold1 (Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "fold1" do
      let r = map fold1 (prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "fold1 from start" do
      let r = map fold1 (prev =<< prev =<< prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r


    test "foldr from the beginning" do
      let r = map (foldr (<>) "") (prev =<< prev =<< prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "foldr" do
      let r = map (foldr (<>) "") (prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "foldr from the end" do
      let r = map (foldr (<>) "") (Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r

    test "foldl from the beginning" do
      let r = map (foldl (<>) "") (prev =<< prev =<< prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "foldl" do
      let r = map (foldl (<>) "") (prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "foldl from the end" do
      let r = map (foldl (<>) "") (Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r


    test "foldMap from the beginning" do
      let r = map (foldMap identity) (prev =<< prev =<< prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "foldMap" do
      let r = map (foldMap identity) (prev =<< prev =<< Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r
    test "foldMap from the end" do
      let r = map (foldMap identity) (Pointed.fromFoldable ["1", "2", "3", "4", "5"])
      equal (Just "12345") r


    test "last" $ do
      let
        arr =  ["1", "2", "3", "4", "5"]
        r = prev =<< Pointed.fromFoldable arr
      equal (Just false) (atEnd <$> r)
      equal (Just true) (atEnd <<< last <$> r)
      equal (Just arr) (Array.fromFoldable <<< last <$> r)
    test "first" $ do
      let
        arr =  ["1", "2", "3", "4", "5"]
        r = prev =<< Pointed.fromFoldable arr
      equal (Just false) (atStart <$> r)
      equal (Just true) (atStart <<< first <$> r)
      equal (Just arr) (Array.fromFoldable <<< first <$> r)
