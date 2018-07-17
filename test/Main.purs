module Test.Main where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Record.Extra (type (:::), SLProxy(..), SNil, compareRecord, keys, mapRecord, sequenceRecord, slistKeys, subsetRecord, zipRecord)
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (equal, shouldEqual)
import Test.Unit.Main (runTest)
import Type.Row (RProxy(..))

main :: Effect Unit
main = runTest do
  suite "Record extras" do
    test "mapRecord" do
      let mapped = mapRecord ((+) 1) {a: 1, b: 2, c: 3}
      equal 2 mapped.a
      equal 3 mapped.b
      equal 4 mapped.c

      let shown = mapRecord show {a: 1, b: 2, c: 3}
      equal "1" shown.a
      equal "2" shown.b
      equal "3" shown.c

    test "zipRecord" do
      let zipped = zipRecord { a: 1, b: 5 } { a: 1, b: 4 }
      equal (Tuple 1 1) zipped.a
      equal (Tuple 5 4) zipped.b

    test "keys" do
      let keyed = keys { a: 1, b: 2 }
      equal ("a" : "b" : Nil) keyed

    test "slistKeys" do
      let slistKeyed = slistKeys $ SLProxy :: SLProxy ("a" ::: "b" ::: "c" ::: SNil)
      equal ("a" : "b" : "c" : Nil) slistKeyed

    test "compareRecord" do
      compareRecord {a: 1, b: 2, c: 3} {a: 1, b: 2, c: 3} `shouldEqual` EQ
      compareRecord {a: 2, b: 2, c: 3} {a: 1, b: 2, c: 3} `shouldEqual` GT
      compareRecord {a: 1, b: 3, c: 3} {a: 1, b: 2, c: 3} `shouldEqual` GT
      compareRecord {a: 1, b: 2, c: 4} {a: 1, b: 2, c: 3} `shouldEqual` GT
      compareRecord {a: 1, b: 2, c: 3} {a: 2, b: 2, c: 3} `shouldEqual` LT
      compareRecord {a: 1, b: 2, c: 3} {a: 1, b: 3, c: 3} `shouldEqual` LT
      compareRecord {a: 1, b: 2, c: 3} {a: 1, b: 2, c: 4} `shouldEqual` LT

    test "subsetRecord" do
       subsetRecord {a: 1, b: "foo"} (RProxy :: RProxy (a :: Int)) `shouldEqual` { a: 1 }
       subsetRecord {a: 1, b: "foo"} (RProxy :: RProxy (a :: Int, b :: String)) `shouldEqual` { a: 1, b: "foo" }
       subsetRecord {a: 1, b: "foo"} (RProxy :: RProxy ()) `shouldEqual` {}

    test "sequenceRecord" do
      let sequenced = sequenceRecord {x: Just "a", y: Just 1, z: Just 3}
      case sequenced of
        Just inner -> do
          equal "a" inner.x
          equal 1 inner.y
          equal 3 inner.z
        Nothing -> do
          failure "sequenceRecord failed"

      let sequenced2 = sequenceRecord {x: Nothing, y: Just 1, z: Just 3}
      case sequenced2 of
        Just _ -> do
          failure "sequenceRecord failed"
        Nothing -> do
          success
