module Sexpr exposing (..)

import Parser as P
import Parser exposing (Parser, (|.), (|=))
import Set as Set
import Http

type Sexpr
  = List (List Sexpr)
  | Symbol String
  | String String
  | Bool Bool
  | Int Int

isSymbol : Char -> Bool
isSymbol c = not (Set.member c (Set.fromList [' ', '\n', '\r', '(', ')', '"']))

stringP : Parser Sexpr
stringP =
  P.succeed String
    |. P.chompIf (\c -> c == '"')
    |= P.loop
         ""
         (\s ->
           P.oneOf
             [ P.succeed (\t -> P.Loop (String.append s t))
                 |. P.chompIf (\c -> c == '\\')
                 |= P.getChompedString (P.chompIf (\_ -> True))
             , P.succeed (P.Done s)
                 |. P.chompIf (\c -> c == '"')
             , P.succeed (\t -> P.Loop (String.append s t)) 
                 |= P.getChompedString (P.chompWhile (\c -> c /= '\\' && c /= '"'))
             ]
         )

sexprP : Parser Sexpr
sexprP =
  P.oneOf
    [ P.map List <| P.sequence
        { start = "("
        , separator = ""
        , end = ")"
        , spaces = P.spaces
        , item = P.lazy (\_ -> sexprP)
        , trailing = P.Forbidden
        }
    , P.map Int P.int
    , P.map Symbol <| P.variable
        { start = isSymbol
        , inner = isSymbol
        , reserved = Set.fromList [ "#t", "#f" ]
        }
    , stringP
    , P.oneOf
        [ P.succeed (Bool True)
            |. P.token "#t"
        , P.succeed (Bool False)
            |. P.token "#f"
        ]
    ]

escapeString : String -> String
escapeString s =
  String.foldr
    (\c t ->
      case c of
        '"' -> String.append "\\\"" t
        '\\' -> String.append "\\\\" t
        _ -> String.cons c t
    )
    ""
    s

toString : Sexpr -> String
toString sexpr =
  case sexpr of
    (List l) -> String.concat (List.concat [["("], List.intersperse " " (List.map toString l), [")"]])
    (Symbol s) -> s
    (String s) -> String.concat ["\"", escapeString s, "\""]
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    (Int n) -> String.fromInt n

expectSexpr : (Result Http.Error Sexpr -> msg) -> Http.Expect msg
expectSexpr f =
  Http.expectString
    (\r ->
      f (Result.andThen
        (\s ->
          case P.run sexprP s of
            (Ok sexpr) -> Ok sexpr
            (Err _) -> Err (Http.BadBody "Failed to parse sexpr")
        )
        r)
    )
