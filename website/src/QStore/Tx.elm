module QStore.Tx exposing (..)

import Http
import QStore.Core as Core
import QStore.Ctx as Ctx exposing (Ctx)
import Sexpr as S exposing (Sexpr)

type Val a
  = Var String
  | Wildcard
  | Val a

valMap : (a -> b) -> Val a -> Val b
valMap f v =
  case v of
    Var x -> Var x
    Wildcard -> Wildcard
    Val a -> Val (f a)

type Slot a = Slot (Val a) (a -> Core.Value)

type alias TxQ = { id : Val Core.Id, attr : Val String, value : Val Core.Value, truth : Bool }

type Tx = Tx (List TxQ)

type TxError = Failed

id : Core.Id -> Slot Core.Id
id x = Slot (Val x) Core.VId

string : String -> Slot String
string s = Slot (Val s) Core.VString

int : Int -> Slot Int
int i = Slot (Val i) Core.VInt

assert : { id : Slot Core.Id, attr : Slot String, value : Slot a } -> Tx
assert q =
  let
    (Slot idv _) = q.id
    (Slot attrv _) = q.attr
    (Slot valuev f) = q.value
  in
    Tx [{ id = idv, attr = attrv, value = valMap f valuev, truth = True }]

retract : { id : Slot Core.Id, attr : Slot String, value : Slot a } -> Tx
retract q =
  let
    (Slot idv _) = q.id
    (Slot attrv _) = q.attr
    (Slot valuev f) = q.value
  in
    Tx [{ id = idv, attr = attrv, value = valMap f valuev, truth = False }]

batch : List Tx -> Tx
batch txs = Tx (List.concat (List.map (\(Tx l) -> l) txs))

tx : String -> (Ctx v, v -> Tx) -> Cmd (Result TxError ())
tx url (ctx, txf) =
  let
    t = txf (Ctx.run ctx)
  in
    Http.post
      { url = url
      , body = Http.stringBody "application/sexpr" (S.toString (txToSexpr t))
      , expect = Http.expectWhatever (\respRes ->
          case respRes of
            Ok resp -> Ok ()
            Err err -> Err Failed
        )
      }

txToSexpr : Tx -> Sexpr
txToSexpr (Tx l) = S.List (List.map txQToSexpr l)

txQToSexpr : TxQ -> Sexpr
txQToSexpr q = S.List [valToSexpr Core.idToSexpr q.id, valToSexpr S.String q.attr, valToSexpr Core.valueToSexpr q.value, S.Bool q.truth]

valToSexpr : (a -> Sexpr) -> Val a -> Sexpr
valToSexpr aToSexpr v =
  case v of
    Var x -> S.Symbol x
    Wildcard -> S.Symbol "_"
    Val a -> aToSexpr a
