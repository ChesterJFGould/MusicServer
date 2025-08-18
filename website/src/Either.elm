module Either exposing ( Either ( .. ), consolidate )

type Either a b = Left a | Right b

consolidate : Either a a -> a
consolidate e =
  case e of
    Left a -> a
    Right a -> a
