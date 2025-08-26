module Album exposing ( Album, Track, query, update, updateTrack, tracksQuery, view, viewTrack, editingView, editingTrackView )

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import QStore.Core as Core
import QStore.Ctx as Ctx exposing (Ctx)
import QStore.Query as Query exposing ( Query )
import QStore.Tx as Tx exposing (Tx)
import Song exposing (Song)

type alias Album =
  { id : Core.Id
  , name : String
  }

type alias Track = { id : Core.Id, number : Int, song : Song }

query : (Ctx (Ctx.Var Core.Id, Ctx.Var String), (Ctx.Var Core.Id, Ctx.Var String) -> Query Album)
query =
  ( Ctx.pure (\id name -> (id, name))
      |> Ctx.app Ctx.idVar
      |> Ctx.app Ctx.stringVar
  , \(id, name) ->
      Query.pure (\a b -> { name = b.value, id = a.id })
        |> Query.app (Query.true { id = Query.var id, attr = Query.string "is", value = Query.string "album", tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var id, attr = Query.string "name", value = Query.var name, tx = Query.wildcard })
  )

update : { was : Album, is : Album } -> (Ctx (), () -> Tx)
update a =
  ( Ctx.pure ()
  , \() ->
      Tx.batch
        [ Tx.assert { id = Tx.id a.is.id, attr = Tx.string "name", value = Tx.string a.is.name }
        , Tx.retract { id = Tx.id a.was.id, attr = Tx.string "name", value = Tx.string a.was.name }
        ]
  )

updateTrack : { was : Track, is : Track } -> (Ctx (), () -> Tx)
updateTrack track =
  ( Ctx.pure ()
  , \() ->
      Tx.batch
        [ Tx.assert { id = Tx.id track.is.id, attr = Tx.string "number", value = Tx.int track.is.number }
        , Tx.retract { id = Tx.id track.was.id, attr = Tx.string "number", value = Tx.int track.was.number }
        , Tx.assert { id = Tx.id track.is.song.id, attr = Tx.string "name", value = Tx.string track.is.song.name }
        , Tx.retract { id = Tx.id track.was.song.id, attr = Tx.string "name", value = Tx.string track.was.song.name }
        ]
  )

type alias TracksQueryContext =
  { track : Ctx.Var Core.Id
  , song : Ctx.Var Core.Id
  , number : Ctx.Var Int
  , name : Ctx.Var String
  , file : Ctx.Var Core.FileHandle
  }

tracksQuery : Album -> (Ctx TracksQueryContext, TracksQueryContext -> Query Track)
tracksQuery album =
  ( Ctx.pure (\track number song name file -> { track = track, number = number, song = song, name = name, file = file })
      |> Ctx.app Ctx.idVar
      |> Ctx.app Ctx.intVar
      |> Ctx.app Ctx.idVar
      |> Ctx.app Ctx.stringVar
      |> Ctx.app Ctx.fileHandleVar
  , \ctx ->
      Query.pure (\a b c d e -> { id = a.value, number = b.value, song = { id = c.value, name = d.value, file = e.value } })
        |> Query.app (Query.true { id = Query.id album.id, attr = Query.string "includes track", value = Query.var ctx.track, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var ctx.track, attr = Query.string "number", value = Query.var ctx.number, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var ctx.track, attr = Query.string "song", value = Query.var ctx.song, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var ctx.song, attr = Query.string "name", value = Query.var ctx.name, tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var ctx.song, attr = Query.string "file", value = Query.var ctx.file, tx = Query.wildcard })
  )

view : { edit : msg, view : msg }
     -> Album -> Html msg
view cfg album =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.em 20)
    , Css.height (Css.em 1.5)
    , Css.displayFlex
    , Css.flexDirection Css.row
    , Css.justifyContent Css.center
    , Css.fontSize (Css.em 1.5)
    ]
    [
    ]
    [ Html.styled Html.div
        [ Css.displayFlex
        , Css.justifyContent Css.center
        , Css.flexGrow (Css.num 1)
        ]
        [ Events.onClick cfg.view
        ]
        [ Html.text album.name
        ]
    , Html.styled Html.div
        [ Css.displayFlex
        , Css.justifyContent Css.center
        ]
        [ Events.onClick cfg.edit
        ]
        [ Html.text "edit"
        ]
    ]

viewTrack : { playSong : msg, edit : msg } -> Track -> Html msg
viewTrack cfg track =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.fontSize (Css.em 1.5)
    , Css.displayFlex
    , Css.flexDirection Css.row
    , Css.justifyContent Css.center
    ]
    []
    [ Html.styled Html.div
        []
        []
        [Html.text (String.append (String.fromInt track.number) ".")]
    , Html.styled Html.div
        [ Css.flexGrow (Css.num 1)
        , Css.width (Css.em 20)
        , Css.justifyContent Css.left
        , Css.textOverflow Css.ellipsis
        , Css.whiteSpace Css.noWrap
        , Css.overflow Css.hidden
        ]
        [ Attr.title track.song.name
        , Events.onClick cfg.playSong
        ]
        [ Html.text track.song.name ]
    , Html.styled Html.div
        [ Css.displayFlex
        , Css.justifyContent Css.center
        ]
        [ Events.onClick cfg.edit
        ]
        [ Html.text "edit"
        ]
    ]

editingTrackView : { updateEdit : Track -> msg, submitEdit : msg } -> Track -> Html msg
editingTrackView cfg track =
  Html.styled Html.form
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.fontSize (Css.em 1.5)
    , Css.displayFlex
    , Css.flexDirection Css.row
    , Css.justifyContent Css.center
    ]
    [ Events.onSubmit cfg.submitEdit
    ]
    [ Html.styled Html.div
        []
        []
        [Html.text (String.append (String.fromInt track.number) ".")]
    , Html.styled Html.input
        [ Css.flexGrow (Css.num 1)
        , Css.width (Css.em 20)
        , Css.justifyContent Css.left
        , Css.textOverflow Css.ellipsis
        , Css.whiteSpace Css.noWrap
        , Css.overflow Css.hidden
        ]
        [ Attr.type_ "text"
        , Attr.value track.song.name
        , Events.onInput (\n -> let song = track.song in cfg.updateEdit { track | song = { song | name = n } })
        ]
        []
    ]

editingView : { updateEdit : Album -> msg, submitEdit : msg } -> Album -> Html msg
editingView cfg album =
  Html.styled Html.form
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.em 20)
    , Css.height (Css.em 1.5)
    , Css.displayFlex
    , Css.justifyContent Css.center
    , Css.fontSize (Css.em 1.5)
    ]
    [ Events.onSubmit cfg.submitEdit
    ]
    [ Html.styled Html.input
        []
        [ Attr.type_ "text"
        , Attr.value album.name
        , Events.onInput (\n -> cfg.updateEdit { album | name = n })
        ]
        []
    ]
  
