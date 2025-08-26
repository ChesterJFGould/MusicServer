module UrlPersistence exposing (app, Model, Msg)

import Browser
import Browser.Navigation as Nav
import Html
import Url exposing (Url)

type Model model = Model { appModel : model, currentUrl : Url, key : Nav.Key }

type Msg msg
  = ForApp msg
  | UrlRequest Browser.UrlRequest
  | UrlChange Url

app :
  { init : flags -> Url -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  , onUrlChange : Url -> msg
  , modelUrl : model -> Url -> Url
  }
  -> Program flags (Model model) (Msg msg)
app cfg =
  Browser.application
    { init = init cfg.init
    , view = view cfg.view
    , update = update cfg.update cfg.modelUrl cfg.onUrlChange
    , subscriptions = subscriptions cfg.subscriptions
    , onUrlRequest = UrlRequest
    , onUrlChange = UrlChange
    }

init : (flags -> Url -> (model, Cmd msg)) -> flags -> Url -> Nav.Key -> (Model model, Cmd (Msg msg))
init i flags url key =
  let
    (appModel, cmd) = i flags url
  in
    ( Model
        { appModel = appModel
        , currentUrl = url
        , key = key
        }
    , cmd
        |> Cmd.map ForApp
    )

view : (model -> Browser.Document msg) -> Model model -> Browser.Document (Msg msg)
view v (Model m) =
  let
    doc = v m.appModel
  in
    { title = doc.title
    , body = (List.map << Html.map) ForApp doc.body
    }

update : (msg -> model -> (model, Cmd msg)) -> (model -> Url -> Url) -> (Url -> msg) -> Msg msg -> Model model -> (Model model, Cmd (Msg msg))
update u urlPath onUrlChange msg (Model m) =
  case msg of
    ForApp appMsg ->
      let
        (newAppModel, cmd) = u appMsg m.appModel
        newUrl = urlPath newAppModel m.currentUrl
      in
        if newUrl == m.currentUrl
        then
          ( Model { m | appModel = newAppModel }
          , cmd
              |> Cmd.map ForApp
          )
        else
          ( Model
              { m
              | appModel = newAppModel
              , currentUrl = newUrl
              }
          , Cmd.batch
              [ cmd
                  |> Cmd.map ForApp
              , Nav.pushUrl m.key (Url.toString newUrl)
              ]
          )
    UrlRequest (Browser.Internal url) ->
      (Model m, Nav.pushUrl m.key (Url.toString url))
    UrlRequest (Browser.External url) ->
      (Model m, Nav.pushUrl m.key url)
    UrlChange newUrl ->
      if newUrl == m.currentUrl
      then (Model m, Cmd.none)
      else update u urlPath onUrlChange (ForApp (onUrlChange newUrl)) (Model { m | currentUrl = newUrl })

subscriptions : (model -> Sub msg) -> Model model -> Sub (Msg msg)
subscriptions s (Model m) =
  s m.appModel
    |> Sub.map ForApp
