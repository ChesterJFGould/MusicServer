module QStore.Ctx exposing (Ctx, Var, stringVar, intVar, idVar, fileHandleVar, pure, app, run, varToString, matchValue)

import QStore.Core as Core exposing (Id, Value, FileHandle)

type Ctx a = Ctx (Int -> (Int, a))

type Var a = Var String (Core.Value -> Maybe a)

stringVar : Ctx (Var String)
stringVar = Ctx (\n -> (n + 1, Var (String.append "x" (String.fromInt n)) valueToString))

intVar : Ctx (Var Int)
intVar = Ctx (\n -> (n + 1, Var (String.append "x" (String.fromInt n)) valueToInt))

idVar : Ctx (Var Id)
idVar = Ctx (\n -> (n + 1, Var (String.append "x" (String.fromInt n)) valueToId))

fileHandleVar : Ctx (Var FileHandle)
fileHandleVar = Ctx (\n -> (n + 1, Var (String.append "x" (String.fromInt n)) valueToFileHandle))

valueToString : Value -> Maybe String
valueToString v =
  case v of
    Core.VString s -> Just s
    _ -> Nothing

valueToId : Value -> Maybe Id
valueToId v =
  case v of
    Core.VId x -> Just x
    _ -> Nothing

valueToInt : Value -> Maybe Int
valueToInt v =
  case v of
    Core.VInt x -> Just x
    _ -> Nothing

valueToFileHandle : Value -> Maybe FileHandle
valueToFileHandle v =
  case v of
    Core.VFileHandle h -> Just h
    _ -> Nothing

pure : a -> Ctx a
pure a = Ctx (\n -> (n, a))

app : Ctx a -> Ctx (a -> b) -> Ctx b
app (Ctx av) (Ctx fv) =
  Ctx (\m ->
    let
      (n, f) = fv m
      (o, a) = av n
    in (o, f a)
  )

run : Ctx a -> a
run (Ctx f) = Tuple.second (f 0)

varToString : Var a -> String
varToString (Var s _) = s

matchValue : Var a -> Value -> Maybe a
matchValue (Var _ f) v = f v
