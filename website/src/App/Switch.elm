module App.Switch exposing ( Model, Msg, Start (..), app, switchToA, switchToB )

import App exposing (App, Addr)
import Html.Styled as Html exposing (Html)

type Start a b
  = StartA a
  | StartB b

mapStartA : (a1 -> a2) -> Start a1 b -> Start a2 b
mapStartA f s =
  case s of
    StartA a -> StartA (f a)
    StartB b -> StartB b

mapStartB : (b1 -> b2) -> Start a b1 -> Start a b2
mapStartB f s =
  case s of
    StartA a -> StartA a
    StartB b -> StartB (f b)

type Model modelA modelB msg msgA msgB
  = IsA { model : modelA, update : msgA -> modelA -> (modelA, Cmd msg), view : modelA -> List (Html msg)}
  | IsB { model : modelB, update : msgB -> modelB -> (modelB, Cmd msg), view : modelB -> List (Html msg)}

type Msg a b msgA msgB
  = SwitchToA a
  | SwitchToB b
  | ForA msgA
  | ForB msgB

addrA : Addr (Msg a b msgA msgB) msgA
addrA = App.addr ForA

addrB : Addr (Msg a b msgA msgB) msgB
addrB = App.addr ForB

app : (a -> App modelA msgE msgA) -> (b -> App modelB msgE msgB) -> Start a b -> App (Model modelA modelB msgE msgA msgB) msgE (Msg a b msgA msgB)
app mkA mkB start here =
  { init =
      start
        |> mapStartA mkA
        |> mapStartB mkB
        |> init here
  , view = view
  , update = update here mkA mkB
  }

init : Addr msg (Msg a b msgA msgB) -> Start (App modelA msg msgA) (App modelB msg msgB) -> (Model modelA modelB msg msgA msgB, Cmd msg)
init here s =
   case s of
     StartA a ->
       let
         ac = a (App.composeAddr here addrA)
       in
         (IsA { model = Tuple.first ac.init, update = ac.update, view = ac.view }, Tuple.second ac.init)
     StartB b ->
       let
         bc = b (App.composeAddr here addrB)
       in
         (IsB { model = Tuple.first bc.init, update = bc.update, view = bc.view }, Tuple.second bc.init)

view : Model modelA modelB msg msgA msgB -> List (Html msg)
view m =
  case m of
    IsA a ->
      a.view a.model
    IsB b ->
      b.view b.model

update : (Addr msg (Msg a b msgA msgB)) -> (a -> App modelA msg msgA) -> (b -> App modelB msg msgB) -> Msg a b msgA msgB -> Model modelA modelB msg msgA msgB -> (Model modelA modelB msg msgA msgB, Cmd msg)
update here mkA mkB msg m =
  case (msg, m) of
    (SwitchToA a, _) -> init here (StartA (mkA a))
    (SwitchToB b, _) -> init here (StartB (mkB b))
    (ForA msga, IsA a) ->
      a.update msga a.model
        |> Tuple.mapFirst (\newAModel -> IsA { a | model = newAModel })
    (ForB msgb, IsB b) ->
      b.update msgb b.model
        |> Tuple.mapFirst (\newBModel -> IsB { b | model = newBModel })
    (_, _) -> (m, Cmd.none)

switchToA : a -> Msg a b msgA msgB
switchToA = SwitchToA

switchToB : b -> Msg a b msgA msgB
switchToB = SwitchToB
