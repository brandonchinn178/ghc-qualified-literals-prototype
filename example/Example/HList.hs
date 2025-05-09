{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Example.HList where

data HList f xs where
  HCons :: f x -> HList f xs -> HList f (x ': xs)
  HNil :: HList f '[]

instance Show (HList f '[]) where
  show HNil = "[]"
instance (Show (f x), Show (HList f xs)) => Show (HList f (x ': xs)) where
  show (HCons x xs) = show x ++ " : " ++ show xs

buildList ::
  ( (forall a as. f a -> HList f as -> HList f (a ': as))
    -> HList f '[]
    -> HList f xs
  ) -> HList f xs
buildList f = f HCons HNil

pattern ListCons :: f x -> HList f xs -> HList f (x ': xs)
pattern ListCons a b = HCons a b

pattern ListNil :: HList f '[]
pattern ListNil = HNil

{-# COMPLETE ListCons, ListNil #-}
