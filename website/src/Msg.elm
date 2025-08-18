module Msg exposing (Msg, ViewId, Model, map, return, andThen, join, view, update, init, query, updateView, newView, playSong)

import Audio
import Html.Styled as Html
import Html.Styled exposing (Html)
import Http
import Types exposing (..)
import QStore.Core as Core
import QStore.Query as Query

type Model = Model { currentView : Html (Msg ()), currentViewId : ViewId }

type Msg a = Msg (Model -> (Model, Turing a))

type ViewId = ViewId Int

viewIdEq : ViewId -> ViewId -> Bool
viewIdEq (ViewId a) (ViewId b) = a == b

incViewId : ViewId -> ViewId
incViewId (ViewId i) = ViewId (i + 1)

type Turing a = Halt a | MoreMsg (Msg a) | MoreCmd (Cmd (Msg a))

mapTuring : (a -> b) -> Turing a -> Turing b
mapTuring f t =
  case t of
    Halt v -> Halt (f v)
    MoreMsg m -> MoreMsg (map f m)
    MoreCmd cmd -> MoreCmd (Cmd.map (\msgA -> map f msgA) cmd)

map : (a -> b) -> Msg a -> Msg b
map f (Msg g) =
  Msg (\m ->
    case g m of
      (n, t) -> (n, mapTuring f t)
  )

return : a -> Msg a
return a = Msg (\m -> (m, Halt a))

andThen : (a -> Msg b) -> Msg a -> Msg b
andThen f (Msg a) =
  Msg (\m ->
    case a m of
      (n, Halt aRes) ->
        case f aRes of
          Msg b -> b n
      (n, MoreMsg o) -> (n, MoreMsg (andThen f o))
      (n, MoreCmd cmd) -> (n, MoreCmd (Cmd.map (\x -> join (map f x)) cmd))
  )

join : Msg (Msg a) -> Msg a
join = andThen (\x -> x)

update : Msg () -> Model -> (Model, Cmd (Msg ()))
update (Msg f) m =
  case f m of
    (n, Halt ()) -> (n, Cmd.none)
    (n, MoreMsg msg) -> update msg n
    (n, MoreCmd cmd) -> (n, cmd)

view : Model -> Html (Msg ())
view (Model m) = m.currentView

init : (ViewId -> (Html (Msg ()), Msg ())) -> (Model, Cmd (Msg ()))
init v =
  let (html, msg) = v (ViewId 0)
  in update msg (Model { currentView = html, currentViewId = ViewId 0 })

databaseUrl : String
databaseUrl = "http://localhost:2718/database/"

filesUrl : String
filesUrl = "http://localhost:2718/files/"

query : (Query.Var v, v -> Query.Query a) -> Msg (Result Query.QueryError (List a))
query q = Msg (\m -> (m, MoreCmd (Query.query (String.append databaseUrl "query") q |> Cmd.map return)))

updateView : ViewId -> Html (Msg ()) -> Msg ()
updateView id html =
  Msg (\(Model m) ->
    if viewIdEq m.currentViewId id
    then (Model { m | currentView = html }, Halt ())
    else (Model m, Halt ())
  )

newView : (ViewId -> (Html (Msg ()), Msg ())) -> Msg ()
newView f =
  Msg (\(Model m) ->
    let
      newViewId = incViewId m.currentViewId
      (v, msg) = f newViewId
    in
      (Model { m | currentView = v, currentViewId = newViewId }, MoreMsg msg)
  )

playSong : Core.FileHandle -> Msg ()
playSong h =
  Msg (\m ->
    ( m
    , MoreCmd (Audio.run (Audio.sequence [Audio.setSource (String.concat [filesUrl, Core.fileHandleToPath h]), Audio.play]))
    )
  )

{-
viewPlayer : Model -> Html Msg
viewPlayer player =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.displayFlex
    , Css.justifyContent Css.center
    , Css.flexShrink (Css.num 1)
    , Css.fontSize (Css.em 2)
    ]
    []
    [ Html.styled Html.div
        [ Css.flexGrow (Css.num 1)
        , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
        , Css.displayFlex
        , Css.justifyContent Css.center
        ]
        [ case player.playing of
            False -> Events.onClick (RunCmd (Audio.run Audio.play))
            True -> Events.onClick (RunCmd (Audio.run Audio.pause))
        ]
        [ case player.playing of
            False -> Html.text "⏵"
            True -> Html.text "⏸"
        ]
    , Html.styled Html.div
        [ Css.flexGrow (Css.num 1)
        , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
        , Css.displayFlex
        , Css.justifyContent Css.center
        ]
        []
        [ Html.text player.song.name ]
    , Html.styled Html.div
        [ Css.flexGrow (Css.num 1)
        , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
        , Css.displayFlex
        , Css.justifyContent Css.center
        ]
        []
        []
    ]
-}
