module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as D

domain : String
domain = ""

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Song = { hash : String, name : String, file : String }

songDecoder : D.Decoder Song
songDecoder =
  D.map3 (\hash name file -> { hash = hash, name = name, file = file })
    (D.field "hash" D.string)
    (D.field "title" D.string)
    (D.field "file" D.string)

songIndexDecoder : D.Decoder (List Song)
songIndexDecoder = D.field "songs" (D.list songDecoder)

type alias Model = List Song

type Msg
  = GotIndex (Result Http.Error (List Song))

init : () -> (Model, Cmd Msg)
init () = ([], Http.get { url= String.append domain "index.json", expect = Http.expectJson GotIndex songIndexDecoder })

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotIndex (Ok songs) -> (songs, Cmd.none)
    GotIndex (Err _) -> (model, Cmd.none)

view : Model -> Html Msg
view model = viewSongList model

viewSongList : List Song -> Html Msg
viewSongList songs = Html.ul [] (List.map viewSong songs)

viewSong : Song -> Html Msg
viewSong song = Html.li [] [ Html.audio [ Attr.src (String.append domain song.file), Attr.controls True ] []]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
