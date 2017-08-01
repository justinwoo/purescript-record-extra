module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Record (delete, get, insert)
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
  , RowLacks name tailRow
  , RowCons name a tailRow row
  , MapRecord tail tailRow a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => MapRecord (Cons name a tail) row a b row' where
  mapRecordImpl _ f r =
    insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r
      r' :: Record tailRow
      r' = delete nameP r
      rest :: Record tailRow'
      rest = mapRecordImpl (RLProxy :: RLProxy tail) f r'

instance mapRecordNil :: MapRecord Nil () a b () where
  mapRecordImpl _ _ _ = {}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ mapRecord ((+) 1) {a: 1, b: 2, c: 3}
  -- {"c":4,"b":3,"a":2}
  print $ mapRecord (append "shown: " <<< show) {a: 1, b: 2, c: 3}
  -- {"c":"3","b":"2","a":"1"}
  where
    print :: forall a. a -> Eff (console :: CONSOLE | e) Unit
    print = log <<< unsafeStringify
