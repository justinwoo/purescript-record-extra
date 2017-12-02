module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Record.Extra (type (:::), SLProxy(..), SNil, appendRecord, eqRecord, keys, mapRecord, sequenceRecord, slistKeys, zipRecord)
import Data.Tuple (Tuple(..))
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (assert, assertFalse, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | e
    )
    Unit
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

    test "eqRecord" do
      assert "works equal" $ eqRecord {a: 1, b: 2, c: 3} {a: 1, b: 2, c: 3}
      assertFalse "works not equal" $ eqRecord {a: 5, b: 2, c: 3} {a: 1, b: 2, c: 3}

    test "appendRecord" do
      let appended = appendRecord {a: "1", b: [2], c: "3"} {a: "a", b: [4], c: "c"}
      equal "1a" appended.a
      equal [2,4] appended.b
      equal "3c" appended.c

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
