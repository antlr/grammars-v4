{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies
             , TypeApplications #-}

module DumpTypecheckedAst where
import Data.Kind

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

data T f (a :: k) = MkT (f a)

type family F (a :: k) (f :: k -> Type) :: Type where
  F @Peano a f = T @Peano f a

main = putStrLn "hello"