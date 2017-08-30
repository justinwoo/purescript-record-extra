module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Record (get, insert)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row (Cons, Nil, kind RowList)

mapRecord :: forall row xs a b row'
   . RowToList row xs
  => MapRecord xs row a b row'
  => (a -> b)
  -> Record row
  -> Record row'
mapRecord = mapRecordImpl (RLProxy :: RLProxy xs)

class MapRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  mapRecordImpl :: RLProxy xs -> (a -> b) -> Record row -> Record row'

instance mapRecordCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , MapRecord tail row a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => MapRecord (Cons name a tail) row a b row' where
  mapRecordImpl _ f r =
    insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r
      rest = mapRecordImpl (RLProxy :: RLProxy tail) f r

instance mapRecordNil :: MapRecord Nil row a b () where
  mapRecordImpl _ _ _ = {}

class ZipRecord
  ( rla :: RowList )
  ( ra :: # Type )
  ( rlb :: RowList )
  ( rb :: # Type )
  ( rc :: # Type )
  | rla -> ra rc
  , rlb -> rb rc
  where
    zipRecordImpl ::
         RLProxy rla
      -> Record ra
      -> RLProxy rlb
      -> Record rb
      -> Record rc

instance zipRecordNil :: ZipRecord Nil trashA Nil trashB ()
  where
    zipRecordImpl _ _ _ _ = {}

instance zipRecordCons
    :: ( IsSymbol k
       , RowCons k a trashA ra
       , RowCons k b trashB rb
       , RowCons k (Tuple a b) rc' rc
       , RowLacks k rc'
       , ZipRecord ta ra tb rb rc'
       )
    => ZipRecord
         (Cons k a ta)
         ra
         (Cons k b tb)
         rb
         rc
  where
    zipRecordImpl _ ra _ rb = insert name head tail
      where
        name = SProxy :: SProxy k
        head = Tuple (get name ra) (get name rb)
        ta = RLProxy :: RLProxy ta
        tb = RLProxy :: RLProxy tb
        tail = zipRecordImpl ta ra tb rb

zipRecord :: forall ta ra tb rb rc a b
   . RowToList ra ta
  => RowToList rb tb
  => ZipRecord ta ra tb rb rc
  => Record ra
  -> Record rb
  -> Record rc
zipRecord ra rb = zipRecordImpl ta ra tb rb
  where
    ta = RLProxy :: RLProxy ta
    tb = RLProxy :: RLProxy tb

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ mapRecord ((+) 1) {a: 1, b: 2, c: 3}
  -- {"c":4,"b":3,"a":2}
  print $ mapRecord (append "shown: " <<< show) {a: 1, b: 2, c: 3}
  -- {"c":"3","b":"2","a":"1"}

  print $ zipRecord { a: 1, b: 5 } { a: 1, b: 4 }
  -- {"b":{"value0":5,"value1":4},"a":{"value0":1,"value1":1}}
  where
    print :: forall a. a -> Eff (console :: CONSOLE | e) Unit
    print = log <<< unsafeStringify
