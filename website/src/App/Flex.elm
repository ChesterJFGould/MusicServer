module App.Flex exposing ( filler, column, row )

import App exposing ( App )
import App.List
import Css
import Html.Styled as Html exposing ( Html )
import ListPos exposing ( ListPos )

filler : App () msg msgI
filler _ =
  { init = ((), Cmd.none)
  , view =
      \_ -> [ Html.styled Html.div [ Css.flexGrow (Css.num 1) ] [] [] ]
  , update = \_ () -> ((), Cmd.none)
  }

flex : Css.FlexDirection d -> App model msgE msgI -> App model msgE msgI
flex d =
  App.wrapView
    (\html ->
      [ Html.styled Html.div
          [ Css.displayFlex
          , Css.flexDirection d
          , Css.justifyContent Css.center
          ]
          []
          html
      ]
    )

column : App model msgE msgI -> App model msgE msgI
column = flex Css.column

row : App model msgE msgI -> App model msgE msgI
row = flex Css.row
