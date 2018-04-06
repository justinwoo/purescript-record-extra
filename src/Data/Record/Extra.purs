module Data.Record.Extra where

import Prelude

import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Record (get, insert)
import Data.Tuple (Tuple(..))
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
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

zipRecord :: forall ta ra tb rb rc
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

class Keys (xs :: RowList) where
  keysImpl :: RLProxy xs -> List String

instance nilKeys :: Keys Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keysImpl (RLProxy :: RLProxy tail)

keys :: forall g row rl
   . RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> List String
keys _ = keysImpl (RLProxy :: RLProxy rl)

slistKeys :: forall g tuples rl
   . SListToRowList tuples rl
  => Keys rl
  => g tuples
  -> List String
slistKeys _ = keysImpl (RLProxy :: RLProxy rl)

foreign import kind SList
foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

data SLProxy (xs :: SList) = SLProxy

infixr 6 type SCons as :::

class SListToRowList (xs :: SList) (rl :: RowList) | xs -> rl, rl -> xs

instance slToRlSNil :: SListToRowList SNil Nil

instance slToRlSCons ::
  ( SListToRowList sTail tail
  ) => SListToRowList (SCons name sTail) (Cons name trash tail)

class EqRecord rl row
  | rl -> row
  where
    eqRecordImpl :: RLProxy rl -> Record row -> Record row -> Boolean

instance eqRecordCons ::
  ( IsSymbol name
  , Eq ty
  , RowCons name ty trash row
  , EqRecord tail row
  ) => EqRecord (Cons name ty tail) row where
  eqRecordImpl _ a b =
    if valA == valB
      then eqRecordImpl tailp a b
      else false
    where
      namep = SProxy :: SProxy name
      valA = get namep a
      valB = get namep b
      tailp = RLProxy :: RLProxy tail

instance eqRecordNil :: EqRecord Nil row where
  eqRecordImpl _ _ _ = true

eqRecord :: forall row rl
   . RowToList row rl
  => EqRecord rl row
  => Record row
  -> Record row
  -> Boolean
eqRecord a b = eqRecordImpl (RLProxy :: RLProxy rl) a b

class Applicative m <= SequenceRecord rl row row' m
  | rl -> row row', rl -> m
  where
    sequenceRecordImpl :: RLProxy rl -> Record row -> m (Record row')

instance sequenceRecordCons ::
  ( IsSymbol name
  , Applicative m
  , RowCons name (m ty) trash row
  , SequenceRecord tail row tailRow' m
  , RowLacks name tailRow'
  , RowCons name ty tailRow' row'
  ) => SequenceRecord (Cons name (m ty) tail) row row' m where
  sequenceRecordImpl _ a  =
       insert namep <$> valA <*> rest
    where
      namep = SProxy :: SProxy name
      valA = get namep a
      tailp = RLProxy :: RLProxy tail
      rest = sequenceRecordImpl tailp a

instance sequenceRecordNil :: Applicative m => SequenceRecord Nil row () m where
  sequenceRecordImpl _ _ = pure {}

sequenceRecord :: forall row row' rl m
   . RowToList row rl
  => Applicative m
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
sequenceRecord a = sequenceRecordImpl (RLProxy :: RLProxy rl) a
