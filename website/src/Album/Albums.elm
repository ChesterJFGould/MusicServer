module Album.Albums exposing ( Model, Msg, app )

import Album exposing ( Album )
import Album.App as Album
import App exposing (App, Addr)
import App.List
import App.Flex as Flex
import Css
import Equiv exposing ( Equiv )
import ListPos
import Html.Styled as Html exposing ( Html )

type Model = Model (App.List.Model Album.Model)

type Msg = Msg (App.List.Msg Album.Msg)

modelEquiv : Equiv (App.List.Model Album.Model) Model
modelEquiv = { from = \(Model l) -> l, to = Model }

msgEquiv : Equiv (App.List.Msg Album.Msg) Msg
msgEquiv = { from = \(Msg l) -> l, to = Msg }

app : Addr msg Album -> List Album -> App Model msg Msg
app albumAddr albums = App.equiv modelEquiv msgEquiv (Flex.column (App.List.app (List.map (Album.app albumAddr) albums)))
