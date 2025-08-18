module App.Product exposing ( Model, Msg, app, addrA, addrB )

import App exposing (App, Addr)
import Html.Styled as Html exposing ( Html )

type Model modelA modelB
  = Model modelA modelB

type Msg msgA msgB
  = ForA msgA
  | ForB msgB

addrA : Addr (Msg msgA msgB) msgA
addrA = App.addr ForA

addrB : Addr (Msg msgA msgB) msgB
addrB = App.addr ForB

app : App modelA msgE msgA -> App modelB msgE msgB -> App (Model modelA modelB) msgE (Msg msgA msgB)
app appA appB here =
  let
    appAc = appA (App.composeAddr here addrA)
    appBc = appB (App.composeAddr here addrB)
  in
    { init = init appAc.init appBc.init
    , view = view appAc.view appBc.view
    , update = update appAc.update appBc.update
    }

init : (modelA, Cmd msg) -> (modelB, Cmd msg) -> (Model modelA modelB, Cmd msg)
init initA initB =
  ( Model (Tuple.first initA) (Tuple.first initB)
  , Cmd.batch [Tuple.second initA, Tuple.second initB]
  )

view : (modelA -> List (Html msg)) -> (modelB -> List (Html msg)) -> Model modelA modelB -> List (Html msg)
view viewA viewB (Model modelA modelB) =
  List.concat
    [ viewA modelA
    , viewB modelB
    ]

update : (msgA -> modelA -> (modelA, Cmd msg)) -> (msgB -> modelB -> (modelB, Cmd msg)) -> Msg msgA msgB -> Model modelA modelB -> (Model modelA modelB, Cmd msg)
update updateA updateB msg (Model modelA modelB) =
  case msg of
    ForA msgA ->
      let
        (newModelA, cmd) = updateA msgA modelA
      in
        ( Model newModelA modelB
        , cmd
        )
    ForB msgB ->
      let
        (newModelB, cmd) = updateB msgB modelB
      in
        ( Model modelA newModelB
        , cmd
        )
