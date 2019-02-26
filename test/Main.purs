module Test.Main where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Record (merge)
import Record.Extra (type (:::), SLProxy(..), SNil, compareRecord, keys, pick, mapRecord, sequenceRecord, slistKeys, zipRecord)
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (equal, shouldEqual)
import Test.Unit.Main (runTest)

-- Need something that is Apply but not Applicative...
newtype Maybe' a = Maybe' (Maybe a)
derive newtype instance maybePrimeFunctor :: Functor Maybe'
derive newtype instance maybePrimeApply :: Apply Maybe'

-- ... and something that is a Functor but not Apply
newtype Maybe'' a = Maybe'' (Maybe a)
derive newtype instance maybe2PrimeFunctor :: Functor Maybe''

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

    test "pick" do
      let r1 = (pick {a: 1, b: 2, c: 3} :: Record (a :: Int, b :: Int))
      r1 `shouldEqual` {a: 1, b: 2}
      let r2 = {a: 4, b: 5, c: 6}
      -- Note: merge copies all runtime keys of an object
      --       which is why we can't just use unsafeCoerce
      (r1 `merge` r2) `shouldEqual` {a: 1, b: 2, c: 6}
      pick {a: 1, b: 2, c: 3} `shouldEqual` {}
      pick {a: 1, b: 2, c: 3} `shouldEqual` {a: 1}
      pick {a: 1, b: 2, c: 3} `shouldEqual` {a: 1, b: 2}
      pick {a: 1, b: 2, c: 3} `shouldEqual` {a: 1, b: 2, c: 3}

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

      let sequenced3 = sequenceRecord {}
      case sequenced3 of
        Just {} -> success
        Nothing -> failure "sequenceRecord failed"

      let sequenced4 = sequenceRecord {x: Maybe' $ Just 1, y: Maybe' $ Just "y"}
      case sequenced4 of
        Maybe' (Just inner) -> do
          equal 1 inner.x
          equal "y" inner.y
        Maybe' Nothing -> failure "sequenceRecord failed"

      let sequenced5 = sequenceRecord {x: Maybe'' $ Just 1}
      case sequenced5 of
        Maybe'' (Just inner) -> equal 1 inner.x
        Maybe'' Nothing -> failure "sequenceRecord failed"
