module Variance where

--                  Position of
--                  a        b
-- Either a b       +        +
-- (a, b)           +        +
-- a -> b           -        +


newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 g) = T1 (f . g)

newtype T2 a = T2 (a -> Int)

instance Functor T2 where
  fmap _f (T2 _g) = undefined

newtype T3 a = T3 (a -> a)

instance Functor T3 where
    fmap _f (T3 _g) = undefined

newtype T4 a = T4 ((Int -> a) -> Int)

instance Functor T4 where
  fmap _f (T4 _g) = undefined


newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 g) = T5 ( \ k -> g (k . f))

newtype Contravariant (f :: * -> *) = Cv  

class Contravariant f where
    contramat :: (a -> b) -> f b -> f a

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

--  Cat       :< Mammal       :< Vertebrate
--  List[Cat] :< List[Mammal] :< List[Vertebrate]
--  -> Cat    :> -> Mammal    :> -> Vertebrate
--
--
-- func(g: Mammal => Mammal)
--
--      OK :
--          f: Vertebrate => Cat
--          f2: Mammal => Mammal
--          f3: Mammal => Cat
--      KO :
--          f3: Cat => Mammal
