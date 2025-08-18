module Equiv exposing ( Equiv, refl, symm )

type alias Equiv a b = { to : a -> b, from : b -> a }

refl : Equiv a a
refl = { to = \x -> x, from = \x -> x }

symm : Equiv a b -> Equiv b a
symm equiv = { to = equiv.from, from = equiv.to }
