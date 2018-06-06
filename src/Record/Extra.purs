module Record.Extra where

import Prelude

import Data.List (List, (:))
import Data.List.Lazy as LL
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as R
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)

mapRecord :: forall row xs a b row'
   . RL.RowToList row xs
  => MapRecord xs row a b row'
  => (a -> b)
  -> Record row
  -> Record row'
mapRecord = mapRecordImpl (RLProxy :: RLProxy xs)

class MapRecord (xs :: RL.RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  mapRecordImpl :: RLProxy xs -> (a -> b) -> Record row -> Record row'

instance mapRecordCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapRecord tail row a b tailRow'
  , Row.Lacks name tailRow'
  , Row.Cons name b tailRow' row'
  ) => MapRecord (RL.Cons name a tail) row a b row' where
  mapRecordImpl _ f r =
    R.insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = f $ R.get nameP r
      rest = mapRecordImpl (RLProxy :: RLProxy tail) f r

instance mapRecordNil :: MapRecord RL.Nil row a b () where
  mapRecordImpl _ _ _ = {}

class ZipRecord
  ( rla :: RL.RowList )
  ( ra :: # Type )
  ( rlb :: RL.RowList )
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

instance zipRecordNil :: ZipRecord RL.Nil trashA RL.Nil trashB ()
  where
    zipRecordImpl _ _ _ _ = {}

instance zipRecordCons
    :: ( IsSymbol k
       , Row.Cons k a trashA ra
       , Row.Cons k b trashB rb
       , Row.Cons k (Tuple a b) rc' rc
       , Row.Lacks k rc'
       , ZipRecord ta ra tb rb rc'
       )
    => ZipRecord
         (RL.Cons k a ta)
         ra
         (RL.Cons k b tb)
         rb
         rc
  where
    zipRecordImpl _ ra _ rb = R.insert name head tail
      where
        name = SProxy :: SProxy k
        head = Tuple (R.get name ra) (R.get name rb)
        ta = RLProxy :: RLProxy ta
        tb = RLProxy :: RLProxy tb
        tail = zipRecordImpl ta ra tb rb

zipRecord :: forall ta ra tb rb rc
   . RL.RowToList ra ta
  => RL.RowToList rb tb
  => ZipRecord ta ra tb rb rc
  => Record ra
  -> Record rb
  -> Record rc
zipRecord ra rb = zipRecordImpl ta ra tb rb
  where
    ta = RLProxy :: RLProxy ta
    tb = RLProxy :: RLProxy tb

class Keys (xs :: RL.RowList) where
  keysImpl :: RLProxy xs -> List String

instance nilKeys :: Keys RL.Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keysImpl (RLProxy :: RLProxy tail)

keys :: forall g row rl
   . RL.RowToList row rl
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

class SListToRowList (xs :: SList) (rl :: RL.RowList) | xs -> rl, rl -> xs

instance slToRlSNil :: SListToRowList SNil RL.Nil

instance slToRlSCons ::
  ( SListToRowList sTail tail
  ) => SListToRowList (SCons name sTail) (RL.Cons name trash tail)

class EqRecord rl row
  | rl -> row
  where
    eqRecordImpl :: RLProxy rl -> Record row -> Record row -> Boolean

instance eqRecordCons ::
  ( IsSymbol name
  , Eq ty
  , Row.Cons name ty trash row
  , EqRecord tail row
  ) => EqRecord (RL.Cons name ty tail) row where
  eqRecordImpl _ a b =
    if valA == valB
      then eqRecordImpl tailp a b
      else false
    where
      namep = SProxy :: SProxy name
      valA = R.get namep a
      valB = R.get namep b
      tailp = RLProxy :: RLProxy tail

instance eqRecordNil :: EqRecord RL.Nil row where
  eqRecordImpl _ _ _ = true

eqRecord :: forall row rl
   . RL.RowToList row rl
  => EqRecord rl row
  => Record row
  -> Record row
  -> Boolean
eqRecord a b = eqRecordImpl (RLProxy :: RLProxy rl) a b

class OrdRecord rl row
  | rl -> row
  where
    compareRecordImpl :: RLProxy rl -> Record row -> Record row -> Ordering

instance ordRecordCons ::
  ( IsSymbol name
  , Ord ty
  , Row.Cons name ty trash row
  , OrdRecord tail row
  ) => OrdRecord (RL.Cons name ty tail) row where
  compareRecordImpl _ a b =
    case compare valA valB of
         EQ -> compareRecordImpl tailp a b
         ordering -> ordering
    where
      namep = SProxy :: SProxy name
      valA = R.get namep a
      valB = R.get namep b
      tailp = RLProxy :: RLProxy tail

instance ordRecordNil :: OrdRecord RL.Nil row where
  compareRecordImpl _ _ _ = EQ

compareRecord :: forall row rl
   . RL.RowToList row rl
  => OrdRecord rl row
  => Record row
  -> Record row
  -> Ordering
compareRecord a b = compareRecordImpl (RLProxy :: RLProxy rl) a b

class ShowRecord rl row | rl -> row where
  showRecordImpl :: RLProxy rl -> Record row -> LL.List String

instance showRecordNil :: ShowRecord RL.Nil () where
  showRecordImpl _ _ = LL.nil

instance showRecordConsShow ::
  ( IsSymbol key
  , Show a
  , ShowRecord listRest rowRest
  , Row.Lacks key rowRest
  , Row.Cons  key a rowRest rowFull
  ) => ShowRecord (RL.Cons key a listRest) rowFull where
  showRecordImpl _ rec = (reflectSymbol key <> ": " <> show val) `LL.cons` rest
    where
    key = SProxy :: SProxy key
    val = R.get key rec
    rest = showRecordImpl (RLProxy :: RLProxy listRest) (R.delete key rec)

showRecord
  :: forall row list
   . RL.RowToList row list
  => ShowRecord list row
  => Record row
  -> String
showRecord rec =
  if LL.length recordStrs == 0
    then "{}"
    else "{ " <> LL.intercalate ", " recordStrs <> " }"
  where
  recordStrs = showRecordImpl (RLProxy ::RLProxy list) rec

class Applicative m <= SequenceRecord rl row row' m
  | rl -> row row', rl -> m
  where
    sequenceRecordImpl :: RLProxy rl -> Record row -> m (Record row')


instance sequenceRecordCons ::
  ( IsSymbol name
  , Applicative m
  , Row.Cons name (m ty) trash row
  , SequenceRecord tail row tailRow' m
  , Row.Lacks name tailRow'
  , Row.Cons name ty tailRow' row'
  ) => SequenceRecord (RL.Cons name (m ty) tail) row row' m where
  sequenceRecordImpl _ a  =
       R.insert namep <$> valA <*> rest
    where
      namep = SProxy :: SProxy name
      valA = R.get namep a
      tailp = RLProxy :: RLProxy tail
      rest = sequenceRecordImpl tailp a

instance sequenceRecordNil :: Applicative m => SequenceRecord RL.Nil row () m where
  sequenceRecordImpl _ _ = pure {}

sequenceRecord :: forall row row' rl m
   . RL.RowToList row rl
  => Applicative m
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
sequenceRecord a = sequenceRecordImpl (RLProxy :: RLProxy rl) a
