module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Css as Css
import Html.Styled as Html
import Html.Styled exposing (Html)
import Html.Styled.Attributes as Attr
import Http
import Json.Decode as D

domain : String
domain = ""

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view >> Html.toUnstyled }

type Hash = Hash String

type alias Song = { hash : Hash, name : String, file : String }

songDecoder : D.Decoder Song
songDecoder =
  D.map3 (\hash name file -> { hash = hash, name = name, file = file })
    (D.field "hash" (D.map Hash D.string))
    (D.field "title" D.string)
    (D.field "file" D.string)

songIndexDecoder : D.Decoder (List Song)
songIndexDecoder = D.field "songs" (D.list songDecoder)

type alias Model = { songs : List Song, currentSong : Maybe Hash }

type Msg
  = GotIndex (Result Http.Error (List Song))

init : () -> (Model, Cmd Msg)
init () =
  ({ songs = [], currentSong = Nothing }
  , Http.get { url= String.append domain "index.json", expect = Http.expectJson GotIndex songIndexDecoder })

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotIndex (Ok songs) -> (songs, Cmd.none)
    GotIndex (Err _) -> (model, Cmd.none)

view : Model -> Html Msg
view model = viewSongList model

viewSongList : List Song -> Html Msg
viewSongList songs =
  Html.styled Html.div
    [ Css.displayFlex
    , Css.flexDirection Css.column
    , Css.height (Css.vh 100)
    ]
    []
    [ Html.div
        [ Attr.css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.center
            , Css.height Css.auto
            , Css.flexGrow (Css.num 1)
            ]
        ]
        [ Html.div
            [ Attr.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.width (Css.pct 50)
                ]
            ]
            (List.map viewSong songs)
        ]
    , Html.styled Html.div
        [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
        , Css.displayFlex
        , Css.justifyContent Css.center
        , Css.flexShrink (Css.num 1)
        ]
        []
        [ Html.text "I am a music control bar ⏸︎" ]
    ]

viewSong : Song -> Html Msg
viewSong song =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.pct 100)
    , Css.displayFlex
    , Css.justifyContent Css.center
    ]
    []
    [ Html.text song.name ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none