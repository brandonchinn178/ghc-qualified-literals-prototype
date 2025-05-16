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

pattern FromListCons :: () => xs ~ (x0 ': xs0) => f x0 -> HList f xs0 -> HList f xs
pattern FromListCons a b = HCons a b

pattern FromListNil :: () => xs ~ '[] => HList f xs
pattern FromListNil = HNil

{-# COMPLETE FromListCons, FromListNil #-}
