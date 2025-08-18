module Views.Album exposing ( view )

import Css
import Html.Styled as Html
import Html.Styled exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Msg
import Msg exposing (Msg)
import QStore.Core as Core
import QStore.Query as Query
import QStore.Query exposing (Query)

type alias Song = { number : Int, name : String, id : Core.Id, handle : Core.FileHandle }

type alias SongQueryEnv =
  { track : Query.QueryVar Core.Id
  , number : Query.QueryVar Int
  , song : Query.QueryVar Core.Id
  , name : Query.QueryVar String
  , handle : Query.QueryVar Core.FileHandle
  }

songQuery : Core.Id -> (Query.Var SongQueryEnv, SongQueryEnv -> Query Song)
songQuery albumId =
  ( Query.varPure (\track number song name handle -> { track = track, number = number, song = song, name = name, handle = handle})
      |> Query.varApp Query.idVar
      |> Query.varApp Query.intVar
      |> Query.varApp Query.idVar
      |> Query.varApp Query.stringVar
      |> Query.varApp Query.fileHandleVar
  , \{ track, number, song, name, handle } ->
      Query.pure (\q1 q2 q3 q4 q5 -> { number = q2.value, name = q4.value, id = q3.value, handle = q5.value })
        |> Query.app (Query.true { id = Query.id albumId, attr = Query.string "includes track", value = Query.var track, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var track, attr = Query.string "number", value = Query.var number, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var track, attr = Query.string "song", value = Query.var song, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var song, attr = Query.string "name", value = Query.var name, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var song, attr = Query.string "file", value = Query.var handle, tx = Query.wildcard })
  )

view : { name : String, id : Core.Id } -> Msg.ViewId -> (Html (Msg ()), Msg ())
view album id =
  ( viewAlbum album.name []
  , Msg.query (songQuery album.id)
      |> Msg.andThen (\songsOrErr ->
           case songsOrErr of
             Ok songs -> Msg.updateView id (viewAlbum album.name (List.sortBy (\song -> song.number) songs))
             Err _ -> Msg.return ()
         )
  )

viewAlbum : String -> List Song -> Html (Msg ())
viewAlbum name songs =
  Html.styled Html.div
    [ Css.displayFlex
    , Css.flexDirection Css.column
    , Css.width (Css.pct 50)
    ]
    []
    (List.map viewSong songs)

viewSong : Song -> Html (Msg ())
viewSong song =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.pct 100)
    , Css.displayFlex
    , Css.justifyContent Css.center
    , Css.fontSize (Css.em 1.5)
    ]
    [ Events.onClick (Msg.playSong song.handle)
    ]
    [ Html.text song.name ]
