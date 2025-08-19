module Album exposing ( Album, Track, query, tracksQuery, view, viewTrack, editingView )

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import QStore.Core as Core
import QStore.Query as Query exposing ( Query )
import Song exposing (Song)

type alias Album =
  { id : Core.Id
  , name : String
  }

type alias Track = { id : Core.Id, number : Int, song : Song }

query : (Query.Var (Query.QueryVar Core.Id, Query.QueryVar String), (Query.QueryVar Core.Id, Query.QueryVar String) -> Query Album)
query =
  ( Query.varPure (\id name -> (id, name))
      |> Query.varApp Query.idVar
      |> Query.varApp Query.stringVar
  , \(id, name) ->
      Query.pure (\a b -> { name = b.value, id = a.id })
        |> Query.app (Query.true { id = Query.var id, attr = Query.string "is", value = Query.string "album", tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var id, attr = Query.string "name", value = Query.var name, tx = Query.wildcard })
  )

type alias TracksQueryContext =
  { track : Query.QueryVar Core.Id
  , song : Query.QueryVar Core.Id
  , number : Query.QueryVar Int
  , name : Query.QueryVar String
  , file : Query.QueryVar Core.FileHandle
  }

tracksQuery : Album -> (Query.Var TracksQueryContext, TracksQueryContext -> Query Track)
tracksQuery album =
  ( Query.varPure (\track number song name file -> { track = track, number = number, song = song, name = name, file = file })
      |> Query.varApp Query.idVar
      |> Query.varApp Query.intVar
      |> Query.varApp Query.idVar
      |> Query.varApp Query.stringVar
      |> Query.varApp Query.fileHandleVar
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

viewTrack : { playSong : msg } -> Track -> Html msg
viewTrack cfg track =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.em 20)
    , Css.fontSize (Css.em 1.5)
    , Css.displayFlex
    , Css.flexDirection Css.row
    , Css.justifyContent Css.center
    ]
    [ Events.onClick cfg.playSong
    ]
    [ Html.styled Html.div
        []
        []
        [Html.text (String.append (String.fromInt track.number) ".")]
    , Html.styled Html.div
        [ Css.flexGrow (Css.num 1)
        , Css.displayFlex
        , Css.justifyContent Css.center
        ]
        []
        [Html.text track.song.name]
    ]


editingView : { updateEdit : Album -> msg, submitEdit : msg } -> Album -> Html msg
editingView cfg album =
  Html.styled Html.form
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.em 20)
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
  
