module App exposing ( App, Addr, Msg (..), run, addr, composeAddr, send, wrapView, equiv )

import Browser
import Either
import Either exposing ( Either )
import Equiv exposing ( Equiv )
import Html.Styled as Html exposing ( Html )
import ListPos exposing ( ListPos (..) )

type Msg msgE msgI
  = External msgE
  | Internal msgI

type Addr msg msgI = Addr (msgI -> msg)

idAddr : Addr msg msg
idAddr = Addr identity

composeAddr : Addr a b -> Addr b c -> Addr a c
composeAddr (Addr ab) (Addr bc) = Addr (bc >> ab)

send : Addr msgE msg -> msg -> msgE
send (Addr f) msg = f msg

addr : (msgI -> msg) -> Addr msg msgI
addr = Addr

type alias App model msgE msgI
  = Addr msgE msgI ->
    { init : (model, Cmd msgE)
    , view : model -> List (Html msgE)
    , update : msgI -> model -> (model, Cmd msgE)
    }

wrapView : (List (Html msg) -> List (Html msg)) -> App model msg msgI -> App model msg msgI
wrapView wrapper app here =
  let
    appc = app here
  in
    { init = appc.init
    , view = appc.view >> wrapper
    , update = appc.update
    }

run : String -> App model msg msg -> Program () model msg
run title app =
  let
    appc = app idAddr
  in
    Browser.document
      { init =
          \() -> appc.init
      , view =
          \model ->
            { title = title
            , body = List.map Html.toUnstyled (appc.view model)
            }
      , update = appc.update
      , subscriptions = \_ -> Sub.none
      }

equiv : Equiv model1 model2 -> Equiv msgI1 msgI2 -> App model1 msg msgI1 -> App model2 msg msgI2
equiv modelEquiv msgEquiv app here =
  let
    appc = app (composeAddr here (addr msgEquiv.to))
  in
    { init =
        appc.init
          |> Tuple.mapFirst modelEquiv.to
    , update =
        \msg2 model2 ->
          appc.update (msgEquiv.from msg2) (modelEquiv.from model2)
            |> Tuple.mapFirst modelEquiv.to
    , view = appc.view << modelEquiv.from
    }

{-
equivInit : Equiv model1 model2 -> Equiv msgE1 msgE2 -> Equiv msgI1 msgI2 -> (model1, Cmd (Msg msgE1 msgI1)) -> (model2, Cmd (Msg msgE2 msgI2))
equivInit modelEquiv msgEEquiv msgIEquiv (model, cmd) =
  ( modelEquiv.to model
  , cmd
      |> Cmd.map (mapExternal msgEEquiv.to)
      |> Cmd.map (mapInternal msgIEquiv.to)
  )

equivUpdate : Equiv model1 model2 -> Equiv msgE1 msgE2 -> Equiv msgI1 msgI2 -> (msgI1 -> model1 -> (model1, Cmd (Msg msgE1 msgI1))) -> (msgI2 -> model2 -> (model2, Cmd (Msg msgE2 msgI2)))
equivUpdate modelEquiv msgEEquiv msgIEquiv update msg model =
  let
    (newModel, cmd) = update (msgIEquiv.from msg) (modelEquiv.from model)
  in
    ( modelEquiv.to newModel
    , cmd
        |> Cmd.map (mapExternal msgEEquiv.to)
        |> Cmd.map (mapInternal msgIEquiv.to)
    )

equivView : Equiv model1 model2 -> Equiv msgE1 msgE2 -> Equiv msgI1 msgI2 -> (model1 -> List (Html (Msg msgE1 msgI1))) -> (model2 -> List (Html (Msg msgE2 msgI2)))
equivView modelEquiv msgEEquiv msgIEquiv view model =
  view (modelEquiv.from model)
    |> List.map (Html.map (mapExternal msgEEquiv.to))
    |> List.map (Html.map (mapInternal msgIEquiv.to))

equiv : Equiv model1 model2 -> Equiv msgE1 msgE2 -> Equiv msgI1 msgI2 -> App model1 msgE1 msgI1 -> App model2 msgE2 msgI2
equiv modelEquiv msgEEquiv msgIEquiv app =
  { init = equivInit modelEquiv msgEEquiv msgIEquiv app.init
  , view = equivView modelEquiv msgEEquiv msgIEquiv app.view
  , update = equivUpdate modelEquiv msgEEquiv msgIEquiv app.update
  }

handle : App model msgI msgI -> App model msgE msgI
handle app =
  { init =
      app.init
        |> (Tuple.mapSecond << Cmd.map) (Internal << consolidate)
  , view = app.view >> (List.map << Html.map) (Internal << consolidate)
  , update =
    \msg m ->
      app.update msg m
        |> (Tuple.mapSecond << Cmd.map) (Internal << consolidate)
  }

handleMsg : (Msg (Msg msgE msgI) msgI) -> Msg msgE msgI
handleMsg m =
  case m of
    (External (External msgE)) -> External msgE
    (External (Internal msgI)) -> Internal msgI
    (Internal msgI) -> Internal msgI
-}
