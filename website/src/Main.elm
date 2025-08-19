module Main exposing (..)

import Album exposing (Album, Track)
import Audio
import Browser
import Css
import Html.Styled as Html exposing (Html)
import QStore.Core as Core
import QStore.Query as Query
import Song exposing (Song)

type alias Model =
  { browser : Browser
  , currentSong : Maybe Song
  }

type Browser
  = Albums { albums : (List Album), editing : Maybe Album }
  | Tracks (List Track)
  | Loading

type Msg
  = ViewAlbum Album
  | ViewAlbums (List Album)
  | ViewTracks (List Track)
  | PlaySong Song
  | EditAlbum Album
  | UpdateAlbum Album

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init () =
  ( { browser = Loading
    , currentSong = Nothing
    }
  , Query.query "http://localhost:2718/database/query" Album.query
      |> Cmd.map (\res ->
           case res of
             Ok albums -> ViewAlbums albums
             Err _ -> ViewAlbums []
         )
  )

view : Model -> Browser.Document Msg
view m =
  { title = "My Music"
  , body = [Html.toUnstyled (viewModel m)]
  }

viewModel : Model -> Html Msg
viewModel m =
  Html.styled Html.div
    [ Css.displayFlex
    , Css.flexDirection Css.column
    , Css.justifyContent Css.center
    , Css.width (Css.vw 100)
    , Css.height (Css.vh 100)
    ]
    []
    [ Html.styled Html.div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.justifyContent Css.center
        , Css.flexGrow (Css.num 1)
        ]
        []
        [ Html.styled Html.div
            [ Css.flexGrow (Css.num 1)
            ]
            []
            []
        , viewBrowser m.browser
        , Html.styled Html.div
            [ Css.flexGrow (Css.num 1)
            ]
            []
            []
        ]
    , viewPlayer m.currentSong
    ]

viewBrowser : Browser -> Html Msg
viewBrowser b =
  case b of
    Albums a ->
      Html.styled Html.div
        []
        []
        ( a.albums
            |> List.map (\album ->
                 case a.editing of
                   Nothing -> Album.view { view = ViewAlbum album, edit = EditAlbum album } album
                   Just editingAlbum ->
                     if Core.idEq editingAlbum.id album.id
                     then Album.editingView { updateEdit = EditAlbum, submitEdit = UpdateAlbum album } editingAlbum
                     else Album.view { view = ViewAlbum album, edit = EditAlbum album } album
               ) 
        )
    Tracks tracks ->
      Html.styled Html.div
        []
        []
        (List.map (\track -> Album.viewTrack { playSong = PlaySong track.song } track) tracks)
    Loading -> Html.text "Loading..."

viewPlayer : Maybe Song -> Html Msg
viewPlayer m =
  case m of
    Just song ->
      Html.styled Html.div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.justifyContent Css.center
        , Css.fontSize (Css.em 2)
        ]
        []
        [ Html.styled Html.div
            [ Css.flexGrow (Css.num 1)
            , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
            , Css.displayFlex
            , Css.justifyContent Css.center
            ]
            []
            [ Html.text "⏵"
            ]
        , Html.styled Html.div
            [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
            , Css.width (Css.em 20)
            , Css.displayFlex
            , Css.justifyContent Css.center
            ]
            []
            [ Html.text song.name
            ]
        , Html.styled Html.div
            [ Css.flexGrow (Css.num 1)
            , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
            , Css.displayFlex
            , Css.justifyContent Css.center
            ]
            []
            []
        ]
    Nothing ->
      Html.styled Html.div
        [ Css.visibility Css.hidden
        ]
        []
        []

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    ViewAlbums albums ->
      ( { m | browser = Albums { albums = albums, editing = Nothing } }
      , Cmd.none
      )
    ViewAlbum album ->
      ( { m | browser = Loading }
      , Query.query "http://localhost:2718/database/query" (Album.tracksQuery album)
          |> Cmd.map (\res ->
               case res of
                 Ok tracks -> ViewTracks tracks
                 Err _ -> ViewTracks []
             )
      )
    ViewTracks tracks ->
      ( { m | browser = Tracks tracks }
      , Cmd.none
      )
    PlaySong song ->
      ( { m | currentSong = Just song }
      , [ Audio.setSource (String.append "http://localhost:2718/files/" (Core.fileHandleToPath song.file))
        , Audio.play
        ]
          |> Audio.sequence
          |> Audio.run
      )
    EditAlbum album ->
      case m.browser of
        Albums a -> ({ m | browser = Albums { a | editing = Just album } }, Cmd.none)
        _ -> (m, Cmd.none)
    UpdateAlbum _ ->
      case m.browser of
        Albums a -> ({ m | browser = Albums { a | editing = Nothing } }, Cmd.none)
        _ -> (m, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

{-
import Album
import Album.Albums as Albums
import Album.Songs
import App.Loading as Loading
import App exposing ( App )
import App.Flex
import App.Switch
import App.Product
import Audio
import Browser
import Debug
import Css as Css
import Dict
import Equiv exposing (Equiv)
import Html.Styled as Html
import Html.Styled exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Http
import Json.Decode as D
import QStore
import QStore exposing (Value(..), QVal(..), QueryQ, Query)
import QStore.Query as Query
import Sexpr
-- import Types exposing (..)
import Views.Albums
import Msg
import Msg exposing (Msg, Model)

domain : String
domain = ""

type Model
  = Model (App.Switch.Model (Loading.Model Albums.Model Msg Albums.Msg) Album.Songs.Model Msg (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg)

type Msg
  = Msg (App.Switch.Msg () Album.Album (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg)

msgEquiv : Equiv Msg (App.Switch.Msg () Album.Album (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg)
msgEquiv = { to = \(Msg m) -> m, from = Msg }

modelEquiv : Equiv (App.Switch.Model (Loading.Model Albums.Model Msg Albums.Msg) Album.Songs.Model Msg (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg) Model
modelEquiv = { to = Model, from = \(Model m) -> m }

mainApp : App Model msgE Msg
mainApp =
  App.Switch.app
    ( \() ->
        Loading.app
          ( Query.query "http://localhost:2718/database/query" Album.query
              |> Cmd.map (\res ->
                   case res of
                     Ok albums -> Albums.app (Msg << App.Switch.switchToB) albums
                     Err _ -> Albums.app (Msg << App.Switch.switchToB) []
                 )
          )
    )
    Album.Songs.app
    (App.Switch.StartA ())
    |> App.equiv modelEquiv Equiv.refl (Equiv.symm msgEquiv)
    |> App.handle

app =
  App.Product.app
    (App.Product.app App.Flex.filler mainApp)
    App.Flex.filler
    |> App.Flex.row

main = App.run "My Music" app
-}
