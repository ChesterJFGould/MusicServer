module Types exposing (Song, Player, Model (..), Msg (..))

import Html.Styled exposing (Html)
import Http
import QStore exposing (Value(..), QVal(..), QueryQ, Query)

type alias Song = { dbId : QStore.Id, name : String, url : String }

type alias Player = { song : Song, playing : Bool }

type ViewId = ViewId Int

type Model = Model { currentView : Model -> Html Msg, songs : List Song, player : Maybe Player }

type Msg
  = GotIndex (Result Http.Error (List Song))
  | PlaySong Song
  | Play
  | Pause
  | ChangeView (Model -> Html Msg)
  -- UpdateView ViewId (Model -> Html Msg)
  -- NewView (ViewId -> Model -> Html Msg)
  -- Starting to look like a monad
  -- Let's do that
  | RunCmd (Cmd Msg)
