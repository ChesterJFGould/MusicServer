module Views.Albums exposing( view )

import Audio
import Css as Css
import Dict
import Html.Styled as Html
import Html.Styled exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Msg
import Msg exposing (Msg)
import QStore.Core as Core
import QStore.Query as Query
import QStore.Query exposing (Query)
import Views.Album

type alias Album = { name : String, id : Core.Id }

albumsQuery : (Query.Var (Query.QueryVar Core.Id, Query.QueryVar String), (Query.QueryVar Core.Id, Query.QueryVar String) -> Query Album)
albumsQuery =
  ( Query.varPure (\id name -> (id, name))
      |> Query.varApp Query.idVar
      |> Query.varApp Query.stringVar
  , \(id, name) ->
      Query.pure (\a b -> { name = b.value, id = a.id })
        |> Query.app (Query.true { id = Query.var id, attr = Query.string "is", value = Query.string "album", tx = Query.wildcard })
        |> Query.app (Query.true { id = Query.var id, attr = Query.string "name", value = Query.var name, tx = Query.wildcard })
  )

view : Msg.ViewId -> (Html (Msg ()), Msg ())
view id =
  ( viewAlbums []
  , Msg.query albumsQuery
      |> Msg.andThen (\respOrErr ->
           case respOrErr of
             Ok albums -> Msg.updateView id (viewAlbums albums)
             Err _ -> Msg.return ()
         )
  )

{-
viewAlbums : List Album -> Html (Msg ())
viewAlbums albums =
  
  Html.styled Html.div
    [ Css.displayFlex
    , Css.flexDirection Css.column
    , Css.height (Css.vh 100)
    ]
    []
    (List.concat
      [ [ Html.div
            [ Attr.css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.justifyContent Css.center
                , Css.height Css.auto
                , Css.flexGrow (Css.num 1)
                , Css.padding (Css.em 1)
                ]
            ]
            [ Html.styled Html.div
                [ Css.flexGrow (Css.num 1)
                ]
                []
                []
            , viewAlbumList albums
            , Html.styled Html.div
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.flexEnd
                , Css.flexGrow (Css.num 1)
                ]
                [ -- Attr.style "align-content" "flex-start"
                ]
                [ Html.styled Html.div
                    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
                    , Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.alignItems Css.center
                    , Css.width (Css.em 2)
                    , Css.height (Css.em 2)
                    ]
                    [ -- Events.onClick (ChangeView addContentView)
                    ]
                    [ Html.text "+" ]
                ]
            ]
        ]
      -- , Maybe.withDefault
      --     []
      --     ( model.player
      --         |> Maybe.map viewPlayer
      --         |> Maybe.map List.singleton
      --     )
      ]
    )
-}

--viewPlayer : Player -> Html Msg
--viewPlayer player =
--  Html.styled Html.div
--    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
--    , Css.displayFlex
--    , Css.justifyContent Css.center
--    , Css.flexShrink (Css.num 1)
--    , Css.fontSize (Css.em 2)
--    ]
--    []
--    [ Html.styled Html.div
--        [ Css.flexGrow (Css.num 1)
--        , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
--        , Css.displayFlex
--        , Css.justifyContent Css.center
--        ]
--        [ case player.playing of
--            False -> Events.onClick (RunCmd (Audio.run Audio.play))
--            True -> Events.onClick (RunCmd (Audio.run Audio.pause))
--        ]
--        [ case player.playing of
--            False -> Html.text "⏵"
--            True -> Html.text "⏸"
--        ]
--    , Html.styled Html.div
--        [ Css.flexGrow (Css.num 1)
--        , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
--        , Css.displayFlex
--        , Css.justifyContent Css.center
--        ]
--        []
--        [ Html.text player.song.name ]
--    , Html.styled Html.div
--        [ Css.flexGrow (Css.num 1)
--        , Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
--        , Css.displayFlex
--        , Css.justifyContent Css.center
--        ]
--        []
--        []
--    ]

viewAlbums : List Album -> Html (Msg ())
viewAlbums albums =
  Html.div
    [ Attr.css
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.width (Css.pct 50)
        ]
    ]
    (List.map viewAlbum albums)

viewAlbum : Album -> Html (Msg ())
viewAlbum album =
  Html.styled Html.div
    [ Css.border3 (Css.px 2) Css.solid (Css.rgb 0 0 0)
    , Css.width (Css.pct 100)
    , Css.displayFlex
    , Css.justifyContent Css.center
    , Css.fontSize (Css.em 1.5)
    ]
    [ Events.onClick (Msg.newView (Views.Album.view { name = album.name, id = album.id }))
    ]
    [ Html.text album.name ]

-- addContentView : Model -> Html Msg
-- addContentView = inputLinkView ""

-- inputLinkView : String -> Model -> Html Msg
-- inputLinkView linkVal (Model model) =
--   Html.styled Html.div
--     [
--     ]
--     []
--     [ Html.form
--         [ Events.onSubmit (ChangeView mainView)
--         ]
--         [ Html.input
--             [ Attr.type_ "text"
--             , Attr.value linkVal
--             , Events.onInput (\newLinkVal -> ChangeView (inputLinkView newLinkVal))
--             ]
--             []
--         ]
--     ]
