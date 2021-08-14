{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module RecordOperations (Mapped(..),
                         MapRecord,
                         MapField(..),
                         mapRecord,
                         RecordSubSet,
                         recordSubSet) where
import GHC.Records
import GHC.Generics

class MapField a where
  -- | The type mapping for the type of a record field.
  type Mapped a :: *
  -- | The mapping operation on the record field.
  mapField :: a -> Mapped a
  
class GenMapRecord f a b where
  genMapRecord :: (forall c.c -> f c) -> a -> b

instance ( MapField (f b)
         , HasField label a b
         , d ~ Mapped (f b)) =>
         GenMapRecord f a
         (S1 ('MetaSel ('Just label) _x _x2 _x3) (Rec0 d) ())
  where
  genMapRecord f a = M1 $ K1 $ mapField $ f ((getField @label) a :: b)

instance ( GenMapRecord f a (b ())
         , GenMapRecord f a (c ())) =>
         GenMapRecord f a ((b :*: c) ())
  where
  genMapRecord f a = genMapRecord f a :*: genMapRecord f a

instance ( GenMapRecord f a (b ())) =>
         GenMapRecord f a (D1 meta (C1 meta2 b) ())
  where
  genMapRecord f a = M1 $ M1 $ genMapRecord f a

-- | the constraint @`MapRecord` f a b@ determines that @a@ can be
-- converted into @b@, using the newtype @f@.
type MapRecord f a b = GenMapRecord f a (Rep b ())

-- | map a polymorphic function over a record.  The function should be
-- implemented as a newtype which is an instance of the `MapField`
-- class, with corresponding type family `Mapped`.  Pass the newtype
-- constructor to the mapRecord function.  For example to "functorize" all fields:
--
-- @
-- data Foo = Foo
--   { foo :: Int
--   , bar :: String
--   } deriving (Show, Generic)
--
-- data FunctorFoo f = FunctorFoo
--   { foo :: f Int
--   , bar :: f String
--   } deriving (Generic)
--
-- newtype ToFunctor (f :: * -> *) a = ToFunctor a
--
-- instance Applicative f => MapField (ToFunctor f a) where
--   type Mapped (ToFunctor f a) = f a
--   mapField (ToFunctor x) = pure x
--
-- functorize :: Applicative f => Foo -> FunctorFoo f
-- functorize = mapRecord ToFunctor
-- @

mapRecord :: forall b f a.(Generic b, MapRecord f a b)
          => (forall c.c -> f c) -> a -> b
mapRecord f x = to (genMapRecord f x :: Rep b ())

class GenRecordSubSet a b where
  genRecordSubSet :: a -> b

instance (HasField label a b) =>
         GenRecordSubSet a
         (S1 ('MetaSel ('Just label) _x _x2 _x3) (Rec0 b) ())
  where
  genRecordSubSet a = M1 $ K1 ((getField @label) a :: b)

instance ( GenRecordSubSet a (b ())
         , GenRecordSubSet a (c ())) =>
         GenRecordSubSet a ((b :*: c) ())
  where
  genRecordSubSet a = genRecordSubSet a :*: genRecordSubSet a

instance (GenRecordSubSet a (b ())) =>
         GenRecordSubSet a (D1 meta (C1 meta2 b) ())
  where
  genRecordSubSet a = M1 $ M1 $ genRecordSubSet a

-- | The constraint @`RecordSubSet` a b@ determines that the fields of
-- `b` are a subset of the fields of `a`.
type RecordSubSet a b = GenRecordSubSet a (Rep b ())

-- | get subset of a record.  For example:
--
-- @  
-- data Foo = Foo
--   { foo :: Int
--   , bar :: String
--   } deriving (Show, Generic)
--
-- data Bar = Bar
--   { bar :: String
--   } deriving (Show, Generic)
--
-- subFoo :: Foo -> Bar
-- subFoo = recordSubSet
-- @

recordSubSet :: forall b a.(Generic b, RecordSubSet a b)
             => a -> b
recordSubSet x = to (genRecordSubSet x :: Rep b ())

