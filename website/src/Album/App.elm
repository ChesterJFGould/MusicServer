module Album.App exposing ( Model, Msg, app )

import Album exposing ( Album )
import App exposing (App, Addr)
import Css
import Html.Styled as Html exposing ( Html )
import Html.Styled.Events as Events
import QStore.Core as Core
import QStore.Query as Query exposing ( Query )

type Model = Model Album

type Msg = Msg Never

app : Addr msg Album -> Album -> App Model msg Msg
app viewAlbumAddr album here =
  { init = (Model album, Cmd.none)
  , view = view viewAlbumAddr
  , update = update
  }

view : Addr msg Album -> Model -> List (Html msg)
view albumAddr (Model album) =
  [ Html.styled Html.div
      [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
      , Css.width (Css.em 20)
      , Css.displayFlex
      , Css.justifyContent Css.center
      , Css.fontSize (Css.em 1.5)
      ]
      [ Events.onClick (App.send albumAddr album)
      ]
      [ Html.text album.name ]
  ]

update : Msg -> Model -> (Model, Cmd msg)
update i (Model album) =
  case i of
    (Msg never) -> Basics.never never
