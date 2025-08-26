module QStore.Query exposing (..)

import Debug
import Dict
import Dict exposing (Dict)
import Http
import QStore.Core as Core
import QStore.Core exposing (Id, Value, FileHandle)
import QStore.Ctx as Ctx exposing (Ctx)
import Sexpr as S
import Sexpr exposing (Sexpr)

type QueryVal a
  = QVar String
  | QWildcard
  | QVal a

type alias QueryQ = { id : QueryVal Id, attr : QueryVal String, value : QueryVal Core.Value, tx : QueryVal Id }

type alias Answer = Dict String Core.Value

type QueryError
  = BadResponse String
  | NetworkError Http.Error
  | ShouldBeImpossible

type QuerySlot t out = QuerySlot (QueryVal t) (QueryVal Value) (Answer -> Result QueryError (Maybe out))

type Query a = Query (List QueryQ) (Answer -> Result QueryError (Maybe a))

pure : a -> Query a
pure a = Query [] (\_ -> Ok (Just a))

resApp : Result err (a -> b) -> Result err a -> Result err b
resApp fr ar =
  case (fr, ar) of
    (Err err, _) -> Err err
    (_, Err err) -> Err err
    (Ok f, Ok a) -> Ok (f a)

maybeApp : Maybe (a -> b) -> Maybe a -> Maybe b
maybeApp mf ma =
  case (mf, ma) of
    (Just f, Just a) -> Just (f a)
    (_, _) -> Nothing

listApp : List (a -> b) -> List a -> List b
listApp fl al =
  case fl of
    [] -> []
    f :: nextfl -> List.append (List.map f al) (listApp nextfl al)

app : Query a -> Query (a -> b) ->  Query b
app (Query aq aproc) (Query fq fproc) =
  Query
    (List.append fq aq)
    (\resp -> resApp (Result.map maybeApp (fproc resp)) (aproc resp))

query : String -> (Ctx v, (v -> Query a)) -> Cmd (Result QueryError (List a))
query url (ctx, qf) =
  let
    (Query q proc) = qf (Ctx.run ctx)
  in
    Http.post
      { url = url
      , body = Http.stringBody "application/sexpr" (S.toString (queryToSexpr q))
      , expect = expectResponse (\respRes ->
          case respRes of
            Ok resp -> Result.map (List.filterMap identity) (sequenceResults (List.map proc resp))
            Err err -> Err (NetworkError err)
        )
      }

-- holdsString : { id : GhostValue Id, attr : GhostValue String, value : GhostValue String } -> Query { id : Value Id, attr : Value String, value : Value String }
-- holdsString q =
--   Query
--     [{id = q.id, attr = q.attr, value = ghostMap Core.VString q.value, tx = GVar "_"}] -- TODO: Handle wildcards better
--     (\resp ->
--       List.map
--         (\answ ->
--           Result.map3
--             (\idM attrM valueM ->
--               Maybe.map3 (\id attr value -> [{ id = id, attr = attr, value = value }])
--                 idM
--                 attrM
--                 valueM
--                 |> Maybe.withDefault []
--             )
--             (reifyId answ q.id)
--             (reifyString answ q.attr)
--             (reifyString answ q.value)
--         )
--         resp
--         |> sequenceResults
--         |> Result.map List.concat
--     )

var : Ctx.Var a -> QuerySlot a a
var x =
  QuerySlot
    (QVar (Ctx.varToString x))
    (QVar (Ctx.varToString x))
    (\answ ->
      Dict.get (Ctx.varToString x) answ
        |> Maybe.map Ok
        |> Maybe.withDefault (Err (BadResponse (String.append "No value returned for variable: " (Ctx.varToString x))))
        |> Result.map (Ctx.matchValue x)
    )

wildcard : QuerySlot v ()
wildcard = QuerySlot QWildcard QWildcard (\_ -> Ok (Just ()))

string : String -> QuerySlot String ()
string s = QuerySlot (QVal s) (QVal (Core.VString s)) (\_ -> Ok (Just ()))

id : Id -> QuerySlot Id ()
id i = QuerySlot (QVal i) (QVal (Core.VId i)) (\_ -> Ok (Just ()))

querySlotToQueryVal : QuerySlot a o -> QueryVal a
querySlotToQueryVal (QuerySlot qv _ _) = qv

querySlotToQueryValValue : QuerySlot a o -> QueryVal Value
querySlotToQueryValValue (QuerySlot _ qv _) = qv

querySlotProc : QuerySlot a o -> Answer -> Result QueryError (Maybe o)
querySlotProc (QuerySlot _ _ proc) = proc

true : { id : QuerySlot Id idOut, attr : QuerySlot String attrOut, value : QuerySlot v valueOut, tx : QuerySlot Id txOut } -> Query { id : idOut, attr : attrOut, value : valueOut, tx : txOut }
true q =
  Query
    [ { id = querySlotToQueryVal q.id
      , attr = querySlotToQueryVal q.attr
      , value = querySlotToQueryValValue q.value
      , tx = querySlotToQueryVal q.tx
      }
    ] -- TODO: Handle wildcards better
    (\answ ->
      Result.map4
        (\idM attrM valueM txM ->
          Maybe.map4 (\x attr value tx -> { id = x, attr = attr, value = value, tx = tx })
            idM
            attrM
            valueM
            txM
        )
        (querySlotProc q.id answ)
        (querySlotProc q.attr answ)
        (querySlotProc q.value answ)
        (querySlotProc q.tx answ)
    )
    {-
    (\resp ->
      List.map
        (\answ ->
          Result.map4
            (\idM attrM valueM txM ->
              Maybe.map4 (\x attr value tx -> [(answ, { id = x, attr = attr, value = value, tx = tx })])
                idM
                attrM
                valueM
                txM
                |> Maybe.withDefault []
            )
            (querySlotProc q.id answ)
            (querySlotProc q.attr answ)
            (querySlotProc q.value answ)
            (querySlotProc q.tx answ)
        )
        resp
        |> sequenceResults
        |> Result.map List.concat
    )
    -}

{-
type Query a = Query (Int -> { gensym : Int, query : List QueryQ, queryValue : a, responseProc : List Answer -> Result QueryError (List a) })

query : String -> Query (Value a) -> Cmd (Result QueryError (List a))
query url (Query q) =
  let qRes = q 0
  in
    Http.post
      { url = url
      , body = Http.stringBody "application/sexpr" (S.toString (queryToSexpr qRes.query))
      , expect = expectResponse (\respRes ->
          case respRes of
            Ok resp -> qRes.responseProc resp |> Result.map (List.map finalizeValue)
            Err err -> Err (NetworkError err)
        )
      }
-}

-- reifyValue : Answer -> GhostValue Core.Value -> Result QueryError (Value Core.Value)
-- reifyValue answ g =
--   case g of
--     GVar x ->
--       Dict.get x answ
--         |> Maybe.map Ok
--         |> Maybe.withDefault (Err (BadResponse (String.append "No value returned for variable: " x)))
--         |> Result.map DuringResponse
--     GVal a -> Ok (DuringResponse a)
-- 
-- reifyId : Answer -> GhostValue Id -> Result QueryError (Maybe (Value Id))
-- reifyId a g =
--   reifyValue a (ghostMap Core.VId g)
--     |> Result.map (\v ->
--          case v of
--            DuringResponse (Core.VId id) -> Just (DuringResponse id)
--            _ -> Nothing
--        )
-- 
-- reifyString : Answer -> GhostValue String -> Result QueryError (Maybe (Value String))
-- reifyString a g =
--   reifyValue a (ghostMap Core.VString g)
--     |> Result.map (\v ->
--          case v of
--            DuringResponse (Core.VString s) -> Just (DuringResponse s)
--            _ -> Nothing
--        )

sequenceResults : List (Result err ok) -> Result err (List ok)
sequenceResults l =
  case l of
    [] -> Ok []
    (res :: ress) -> Result.map2 (\resOk ressOk -> resOk :: ressOk) res (sequenceResults ress)

sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe l =
  case l of
    [] -> Just []
    (res :: ress) -> Maybe.map2 (\resOk ressOk -> resOk :: ressOk) res (sequenceMaybe ress)

{-
holdsString : { id : GhostValue Id, attr : GhostValue String, value : GhostValue String } -> Query { id : Value Id, attr : Value String, value : Value String }
holdsString q =
  Query (\n ->
    { gensym = n
    , query = [{id = q.id, attr = q.attr, value = ghostMap Core.VString q.value, tx = GVar "_"}] -- TODO: Deal with wildcards better
    , queryValue = { id = DuringQuery, attr = DuringQuery, value = DuringQuery }
    , responseProc = \resp ->
        List.map (\a ->
          Result.map3
            (\idM attrM valueM ->
              Maybe.map3 (\id attr value -> [{ id = id, attr = attr, value = value }])
                idM
                attrM
                valueM
                |> Maybe.withDefault []
            )
            (reifyId a q.id)
            (reifyString a q.attr)
            (reifyString a q.value)
        )
        resp
        |> sequenceResults
        |> Result.map List.concat
    }
  )

valueMap : (a -> b) -> Value a -> Value b
valueMap f v =
  case v of
    DuringQuery -> DuringQuery
    DuringResponse a -> DuringResponse (f a)
-}

-- valueMap2 : (a -> b -> c) -> Value a -> Value b -> Value c
-- valueMap2 f va vb =
--   case (va, vb) of
--     (DuringQuery, _) -> DuringQuery
--     (_, DuringQuery) -> DuringQuery
--     (DuringResponse a, DuringResponse b) -> DuringResponse (f a b)
{-
return : a -> Query a
return a =
  Query (\n ->
    { gensym = n
    , query = []
    , queryValue = a
    , responseProc = \_ -> Ok [a]
    }
  )

andThen : (a -> Query b) -> Query a -> Query b
andThen f (Query a) =
  Query (\gensym ->
    let
      aRes = a gensym
      (Query fQRes) = f a.queryValue
      fDuringQuery = fQRes aRes.gensym
      fRespProc =
        \resp ->
          Result.map (f (aRes.responseProc answ))
            |> Result.andThen (\(Query fRRes) -> fRRes.responseProc answ)
    in { gensym = fDuringQuery.gensym, query = List.append aRes.query fDuringQuery.query, queryValue = fDuringQuery.queryValue, responseProc = fRespProc }
  )

map : (a -> b) -> Query a -> Query b
map f aq = andThen (\a -> return (f a)) aq

map2 : (a -> b -> c) -> Query a -> Query b -> Query c
map2 f aq bq =
  andThen (\a -> andThen (\b -> return f a b) bq) aq

var : Query (GhostValue a)
var =
  Query (\gensym ->
    let (newGensym, x) = doGensym gensym
    in { gensym = newGensym, query = [], queryValue = GVar x, responseProc = \answ -> Ok [GVar x] }
  )

doGensym : Int -> (Int, String)
doGensym n = (n + 1, String.append "x" (String.fromInt n))
-}

-- string : String -> GhostValue String
-- string s = GVal s
-- 
-- ghostMap : (a -> b) -> GhostValue a -> GhostValue b
-- ghostMap f g =
--   case g of
--     GVar x -> GVar x
--     GVal a -> GVal (f a)

queryValueToSexpr : (a -> Sexpr) -> QueryVal a -> Sexpr
queryValueToSexpr aToSexpr g =
  case g of
    QVar x -> S.Symbol x
    QWildcard -> S.Symbol "_"
    QVal a -> aToSexpr a

queryQToSexpr : QueryQ -> Sexpr
queryQToSexpr q =
  S.List
    [ queryValueToSexpr Core.idToSexpr q.id
    , queryValueToSexpr S.String q.attr
    , queryValueToSexpr Core.valueToSexpr q.value
    , queryValueToSexpr Core.idToSexpr q.tx
    ]

queryToSexpr : List QueryQ -> Sexpr
queryToSexpr l = S.List (List.map queryQToSexpr l)

answerFromSexpr : Sexpr -> Maybe Answer
answerFromSexpr sexpr =
  case sexpr of
    (S.List s) ->
      s
        |> List.map
             (\pair ->
               case pair of
                 (S.List [S.Symbol x, valS]) -> Maybe.map (\val -> (x, val)) (Core.valueFromSexpr valS)
                 _ -> Nothing
             )
        |> sequenceMaybe
        |> Maybe.map Dict.fromList
    _ -> Nothing

responseFromSexpr : Sexpr -> Maybe (List Answer)
responseFromSexpr s =
  case s of
    S.List l -> sequenceMaybe (List.map answerFromSexpr l)
    _ -> Nothing

expectResponse : (Result Http.Error (List Answer) -> msg) -> Http.Expect msg
expectResponse f =
  S.expectSexpr (\sRes ->
    Result.map responseFromSexpr sRes
      |> Result.andThen (\m ->
           Maybe.map Ok m
           |> Maybe.withDefault (Err (Http.BadBody "Failed to parse response"))
         )
      |> f
  )
