module App.Loading exposing ( Model, Msg, app )

import App exposing (App, Addr)
import Html.Styled as Html exposing ( Html )

type Model model msg msgI
  = Loading
  | Loaded { model : model, view : model -> List (Html msg), update : msgI -> model -> (model, Cmd msg) }

type Msg model msgE msgI
  = Complete (App model msgE msgI)
  | Msg msgI

app : (Cmd (App model msgE msgI)) -> App (Model model msgE msgI) msgE (Msg model msgE msgI)
app loadApp here =
  { init = (Loading, Cmd.map (App.send here << Complete) loadApp)
  , view = view
  , update = update here
  }

view : Model model msg msgI -> List (Html msg)
view m =
  case m of
    Loading ->
      [ Html.styled Html.div
          []
          []
          [ Html.text "Loading..." ]
      ]
    Loaded a -> a.view a.model

update : Addr msg (Msg model msg msgI) -> Msg model msg msgI -> Model model msg msgI -> (Model model msg msgI, Cmd msg)
update here msg model =
  case (msg, model) of
    (Complete a, Loading) ->
      let
        ac = a (App.composeAddr here (App.addr Msg))
        (initm, initcmd) = ac.init
      in
        (Loaded { model = initm, view = ac.view, update = ac.update }, initcmd)
    (Msg im, Loaded a) ->
      let
        (newm, cmd) = a.update im a.model
      in
        (Loaded { a | model = newm }, cmd)
    (_, _) -> (model, Cmd.none)
