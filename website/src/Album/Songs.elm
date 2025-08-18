module Album.Songs exposing ( Model, Msg, app )

import Album exposing (Album)
import App exposing (App)
import Html.Styled as Html exposing ( Html )

type Model = Model

type Msg = Msg

app : Album -> App Model msgE Msg
app album here =
  { init = init album
  , view = view album
  , update = update album
  }

init : Album -> (Model, Cmd msg)
init _ = (Model, Cmd.none)

view : Album -> Model -> List (Html msg)
view album _ =
  [ Html.text album.name
  ]

update : Album -> Msg -> Model -> (Model, Cmd msg)
update _ _ m = (m, Cmd.none)
