module QStore.Core exposing (Id, FileHandle, Value(..), idToSexpr, idFromSexpr, valueToSexpr, valueFromSexpr, fileHandleToPath, idEq)

import Sexpr as S
import Sexpr exposing (Sexpr)

type Id = Id Int

type FileHandle = SHA256HashBase16 String

type Value
  = VId Id
  | VString String
  | VInt Int
  | VFileHandle FileHandle

idEq : Id -> Id -> Bool
idEq (Id a) (Id b) = a == b

idToSexpr : Id -> Sexpr
idToSexpr (Id i) = S.List [S.Symbol "id", S.Int i]

idFromSexpr : Sexpr -> Maybe Id
idFromSexpr s =
  case s of
    S.List [S.Symbol "id", S.Int id] -> Just (Id id)
    _ -> Nothing

valueToSexpr : Value -> Sexpr
valueToSexpr v =
  case v of
    VId id -> idToSexpr id
    VString s -> S.String s
    VInt n -> S.Int n
    VFileHandle (SHA256HashBase16 hash) -> S.List [S.Symbol "file", S.String hash]

valueFromSexpr : Sexpr -> Maybe Value
valueFromSexpr s =
  case s of
    S.String str -> Just (VString str)
    S.Int n -> Just (VInt n)
    S.List [S.Symbol "file", S.String hash] -> Just (VFileHandle (SHA256HashBase16 hash))
    _ -> Maybe.map VId (idFromSexpr s)

fileHandleToPath : FileHandle -> String
fileHandleToPath (SHA256HashBase16 hash) = hash
