module Main exposing (..)

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
