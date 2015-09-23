module lista4.hs where

import Data.Monoid
-- Andrei Martins Silva - 1210045-7 Noturno
-- Carlos Augusto Silva - 1120048-5 Noturno

data Coisa a = Nada 
	        | UmaCoisa a 
	        | DuasCoisas a a 
	        deriving Show

instance (Monoid a) => Monoid (Bolsa a) where
	mempty = Nada
	mappend (Coisa x) ( Coisa y z ) = Coisa x y z
	mappend (Coisa x y) (Coisa z) = Coisa x y z
	mappend (Coisa k l) (Coisa m n) =  Coisa (k <> m) (l <> n)
	mappend (Coisa p q r) (Coisa s t u) = Coisa (p <> s) (q <> t) (r <> u)
	mappend (Coisa a) (Coisa b) = Coisa a b
	mappend (Coisa a) (Coisa b c d) = Coisa (a <> b) c d
	mappend (Coisa a b c) (Coisa d) = Coisa (a <> d) b c
	mappend (Coisa a b) (Coisa c d e) = Coisa (a <> c) (b <> d) e
	mappend (Coisa a b c) (Coisa d e) = Coisa (a <> d) (b <> e) c
	mappend (Coisa a) Nada = Coisa a
	mappend (Coisa a b) Nada = Coisa a b
	mappend (Coisa a b c) Nada = Coisa a b c
	mappend Nada (Coisa a) = Coisa a
	mappend Nada (Coisa a b) = Coisa a b
	mappend Nada (Coisa a b c) = Coisa a b c
	mappend _ _ = Nada



-- Exercício 2


-- Exeercício 3
combinarTudo :: [Coisa a]  -> Coisa a
combinarTudo  







