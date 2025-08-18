module App.List exposing ( Model, Msg, app )

import App exposing (App, Addr)
import Html.Styled as Html exposing (Html)
import ListPos exposing (ListPos)

type Model model = Model (List model)

type Msg msg = Msg (ListPos msg)

addrs : Addr msg (Msg msgI) -> Addr (Msg msgI) msgI -> List (App model msg msgI) -> List { init : (model, Cmd msg), view : model -> List (Html msg), update : msgI -> model -> (model, Cmd msg) }
addrs here pos apps =
  case apps of
    [] -> []
    (a :: napps) -> a (App.composeAddr here pos) :: addrs here (App.composeAddr (App.addr (\(Msg lp) -> Msg (ListPos.Next lp))) pos) napps

app : List (App model msg msgI) -> App (Model model) msg (Msg msgI)
app apps here =
  let
    appcs = addrs here (App.addr (Msg << ListPos.Here)) apps
  in
    { init = init (List.map .init appcs)
    , view = view (List.map .view appcs)
    , update = update (List.map .update appcs)
    }

init : List (model, Cmd msg) -> (Model model, Cmd msg)
init l =
  case l of
    [] ->
      (Model [], Cmd.none)
    ((model, cmd) :: inits) ->
      let
        (Model models, cmds) = init inits
      in
        ( Model (model :: models)
        , Cmd.batch [cmd, cmds]
        )

view : List (model -> List (Html msg)) -> Model model -> List (Html msg)
view views (Model models) = List.concat (List.map2 (\v m -> v m) views models)

update : List (msgI -> model -> (model, Cmd msg)) -> Msg msgI -> Model model -> (Model model, Cmd msg)
update updates (Msg lp) (Model models) =
  case (updates, lp, models) of
    (u :: _, ListPos.Here msg, m :: ms) ->
      u msg m
        |> Tuple.mapFirst (\nm -> Model (m :: ms))
    (_ :: us, ListPos.Next nlp, m :: ms) ->
      update us (Msg nlp) (Model ms)
        |> Tuple.mapFirst (\(Model nms) -> Model (m :: nms))
    (_, _, _) -> (Model models, Cmd.none)
