module QStore exposing (..)

import Dict
import Dict exposing (Dict)
import Http
import Sexpr as S
import Sexpr exposing (Sexpr)
import Parser as P
import Result

type Id = Id Int

type Value
  = VId Id
  | VString String
  | VInt Int

type QVal a
  = QVar String
  | QWildcard
  | QVal a

type alias QueryQ = { id : QVal Id, attr : String, val : QVal Value, tx : QVal Id }

type alias Query = List QueryQ

type alias Answer = Dict String Value

type alias Response = List Answer

query : { url : String, query : Query, result : Result Http.Error Response -> msg } -> Cmd msg
query q =
  Http.post
    { url = q.url
    , body = Http.stringBody "application/sexpr" (S.toString (queryToSexpr q.query))
    , expect =
        S.expectSexpr
          (\res ->
            q.result <| Result.andThen
              (\s ->
                case sexprToResponse s of
                  Nothing -> Err (Http.BadBody "Failed to parse response")
                  Just r -> Ok r
              )
            res
          )
    }

valueToString : Value -> Maybe String
valueToString value =
  case value of
    VString s -> Just s
    _ -> Nothing

valueToId : Value -> Maybe Id
valueToId value =
  case value of
    VId x -> Just x
    _ -> Nothing

idToSexpr : Id -> Sexpr
idToSexpr id =
  case id of
    (Id x) -> S.List [S.Symbol "id", S.Int x]

valueToSexpr : Value -> Sexpr
valueToSexpr value =
  case value of
    (VId id) -> idToSexpr id
    (VString s) -> S.String s
    (VInt n) -> S.Int n

qValToSexpr : (a -> Sexpr) -> QVal a -> Sexpr
qValToSexpr aToSexpr qVal =
  case qVal of
    (QVar x) -> S.Symbol x
    (QWildcard) -> S.Symbol "_"
    (QVal a) -> aToSexpr a

queryQToSexpr : QueryQ -> Sexpr
queryQToSexpr q =
  S.List
    [ qValToSexpr idToSexpr q.id
    , S.String q.attr
    , qValToSexpr valueToSexpr q.val
    , qValToSexpr idToSexpr q.tx
    ]

queryToSexpr : Query -> Sexpr
queryToSexpr q = S.List (List.map queryQToSexpr q)

sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe l =
  case l of
    [] -> Just []
    (res :: ress) -> Maybe.map2 (\resOk ressOk -> resOk :: ressOk) res (sequenceMaybe ress)

sexprToResponse : Sexpr -> Maybe Response
sexprToResponse sexpr =
  case sexpr of
    (S.List l) -> sequenceMaybe (List.map sexprToAnswer l)
    _ -> Nothing

sexprToAnswer : Sexpr -> Maybe Answer
sexprToAnswer sexpr =
  case sexpr of
    (S.List s) ->
      s
        |> List.map
             (\pair ->
               case pair of
                 (S.List [S.Symbol x, valS]) -> Maybe.map (\val -> (x, val)) (sexprToVal valS)
                 _ -> Nothing
             )
        |> sequenceMaybe
        |> Maybe.map Dict.fromList
    _ -> Nothing

sexprToVal : Sexpr -> Maybe Value
sexprToVal sexpr =
  case sexpr of
    (S.List [S.Symbol "id", S.Int x]) -> Just (VId (Id x))
    (S.String s) -> Just (VString s)
    (S.Int n) -> Just (VInt n)
    _ -> Nothing
