module Main exposing (..)

import Album exposing (Album, Track)
import Audio
import Browser
import Browser.Navigation as Nav
import Css
import Debug
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import QStore.Core as Core
import QStore.Query as Query
import QStore.Tx as Tx
import Song exposing (Song)
import Url exposing (Url)
import Url.Builder
import Url.Parser as UrlP exposing ((</>))
import UrlPersistence

type alias Model =
  { browser : Browser
  , player : Maybe Player
  }

type alias Player =
  { song : Song
  , playing : Bool
  }

type Browser
  = Albums { albums : List Album, editing : Maybe Album }
  | Tracks { tracks : List Track, editing : Maybe Track, from : Core.Id }
  | Loading

type Msg
  = ViewAlbum Album
  | ViewAlbums (List Album)
  | ViewTracks (List Track) Core.Id
  | PlaySong Song
  | EditAlbum Album
  | UpdateAlbum { was : Album, is : Album }
  | UpdatedAlbum
  | UpdatedTrack
  | Play
  | Pause
  | EditTrack Track
  | UpdateTrack { was : Track, is : Track }
  | UrlChange Url

type Route
  = AlbumsRoute
  | TracksRoute Core.Id

dbUrl : String
dbUrl = "http://192.168.0.26:2718/database"

filesUrl : String
filesUrl = "http://192.168.0.26:2718/files/"

main : Program () (UrlPersistence.Model Model) (UrlPersistence.Msg Msg)
main =
  UrlPersistence.app
    { init = init
    , onUrlChange = UrlChange
    , view = view
    , update = update
    , modelUrl = modelUrl
    , subscriptions = subscriptions
    }

init : () -> Url -> (Model, Cmd Msg)
init () url =
  case UrlP.parse routeParser url of
    Just (TracksRoute id) ->
      ( { browser = Loading, player = Nothing }
      , Query.query (String.append dbUrl "/query") (Album.tracksQuery { id = id, name = "" }) -- TODO: Do something better
          |> Cmd.map (\res ->
               case res of
                 Ok tracks -> ViewTracks tracks id
                 Err _ -> ViewTracks [] id
             )
      )
    _ ->
      ( { browser = Loading
        , player = Nothing
        }
      , Query.query (String.append dbUrl "/query") Album.query
          |> Cmd.map (\res ->
               case res of
                 Ok albums -> ViewAlbums albums
                 Err _ -> ViewAlbums []
             )
      )

modelUrl : Model -> Url -> Url
modelUrl m url =
  case m.browser of
    Albums _ -> { url | path = Url.Builder.absolute [ "albums" ] [] }
    Tracks t -> { url | path = Url.Builder.absolute [ "albums", String.fromInt (Core.idToInt t.from) ] [] }
    Loading -> url

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
    , Css.overflow Css.hidden
    ]
    []
    [ Html.styled Html.div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.justifyContent Css.center
        , Css.flexGrow (Css.num 1)
        , Css.overflow Css.hidden
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
    , viewPlayer m.player
    ]

viewBrowser : Browser -> Html Msg
viewBrowser b =
  case b of
    Albums a ->
      Html.styled Html.div
        [ Css.overflow Css.hidden
        ]
        []
        ( a.albums
            |> List.map (\album ->
                 case a.editing of
                   Nothing -> Album.view { view = ViewAlbum album, edit = EditAlbum album } album
                   Just editingAlbum ->
                     if Core.idEq editingAlbum.id album.id
                     then Album.editingView { updateEdit = EditAlbum, submitEdit = UpdateAlbum { was = album, is = editingAlbum } } editingAlbum
                     else Album.view { view = ViewAlbum album, edit = EditAlbum album } album
               ) 
        )
    Tracks t ->
      Html.styled Html.div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.overflow Css.scroll
        ]
        []
        ( t.tracks
            |> List.map (\track ->
                 case t.editing of
                   Nothing -> Album.viewTrack { playSong = PlaySong track.song, edit = EditTrack track } track
                   Just editedTrack ->
                     if Core.idEq editedTrack.id track.id
                     then Album.editingTrackView { updateEdit = EditTrack, submitEdit = UpdateTrack { was = track, is = editedTrack } } editedTrack
                     else Album.viewTrack { playSong = PlaySong track.song, edit = EditTrack track } track
               )
        )
    Loading -> Html.text "Loading..."

viewPlayer : Maybe Player -> Html Msg
viewPlayer m =
  case m of
    Just p ->
      Html.styled Html.div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.fontSize (Css.em 2)
        ]
        []
        [ case p.playing of
            True ->
              Html.styled Html.div
                [ Css.flexGrow (Css.num 1)
                , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
                , Css.displayFlex
                , Css.justifyContent Css.center
                ]
                [ Events.onClick Pause
                ]
                [ Html.text "⏸"
                ]
            False ->
              Html.styled Html.div
                [ Css.flexGrow (Css.num 1)
                , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
                , Css.displayFlex
                , Css.justifyContent Css.center
                ]
                [ Events.onClick Play
                ]
                [ Html.text "⏵"
                ]
        , Html.styled Html.div
            [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
            , Css.width (Css.em 20)
            , Css.textAlign Css.center
            , Css.textOverflow Css.ellipsis
            , Css.whiteSpace Css.noWrap
            , Css.overflow Css.hidden
            ]
            [ Attr.title p.song.name
            ]
            [ Html.text p.song.name
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
      , Query.query (String.append dbUrl "/query") (Album.tracksQuery album)
          |> Cmd.map (\res ->
               case res of
                 Ok tracks -> ViewTracks tracks album.id
                 Err _ -> ViewTracks [] album.id
             )
      )
    ViewTracks tracks id ->
      ( { m | browser = Tracks { tracks = tracks, editing = Nothing, from = id } }
      , Cmd.none
      )
    PlaySong song ->
      ( { m | player = Just { song = song, playing = True } }
      , [ Audio.setSource (String.append filesUrl (Core.fileHandleToPath song.file))
        , Audio.play
        ]
          |> Audio.sequence
          |> Audio.run
      )
    EditAlbum album ->
      case m.browser of
        Albums a -> ({ m | browser = Albums { a | editing = Just album } }, Cmd.none)
        _ -> (m, Cmd.none)
    EditTrack track ->
      case m.browser of
        Tracks t -> ({ m | browser = Tracks { t | editing = Just track } }, Cmd.none)
        _ -> (m, Cmd.none)
    UpdateAlbum album ->
      ( case m.browser of
          Albums a ->
            ( { m | browser = Albums { a | editing = Nothing } }
            , Tx.tx (String.append dbUrl "/transact") (Album.update album)
                |> Cmd.map (\res ->
                     case res of
                       Ok () -> UpdatedAlbum
                       Err _ -> UpdatedAlbum
                   )
            )
          _ -> (m, Cmd.none)
      )
    UpdateTrack track ->
      case m.browser of
        Tracks t ->
          ( { m | browser = Tracks { t | editing = Nothing } }
          , Tx.tx (String.append dbUrl "/transact") (Album.updateTrack track)
              |> Cmd.map (\res -> UpdatedTrack)
          )
        _ -> (m, Cmd.none)
    UpdatedAlbum -> (m, Cmd.none)
    UpdatedTrack -> (m, Cmd.none)
    Play ->
      case m.player of
        Just p ->
          ( { m | player = Just { p | playing = True } }
          , Audio.run Audio.play
          )
        Nothing -> (m, Cmd.none)
    Pause ->
      case m.player of
        Just p ->
          ( { m | player = Just { p | playing = False } }
          , Audio.run Audio.pause
          )
        Nothing -> (m, Cmd.none)
    UrlChange u ->
      case UrlP.parse routeParser u of
        Nothing -> (m, Cmd.none)
        Just AlbumsRoute ->
          ( { m | browser = Loading }
          , Query.query (String.append dbUrl "/query") Album.query
              |> Cmd.map (\res ->
                   case res of
                     Ok albums -> ViewAlbums albums
                     Err _ -> ViewAlbums []
                 )
          )
        Just (TracksRoute id) ->
          ( { m | browser = Loading }
          , Query.query (String.append dbUrl "/query") (Album.tracksQuery { id = id, name = "" }) -- TODO: Do something better
              |> Cmd.map (\res ->
                   case res of
                     Ok tracks -> ViewTracks tracks id
                     Err _ -> ViewTracks [] id
                 )
          )

routeParser : UrlP.Parser (Route -> a) a
routeParser =
  UrlP.oneOf
    [ UrlP.map AlbumsRoute (UrlP.s "albums")
    , UrlP.map TracksRoute (UrlP.s "albums" </> UrlP.map Core.idFromInt UrlP.int)
    ]

albumsPath : String
albumsPath = Url.Builder.absolute [ "albums" ] []

albumPath : Album -> String
albumPath a = Url.Builder.absolute [ "albums", String.fromInt (Core.idToInt a.id) ] []

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
