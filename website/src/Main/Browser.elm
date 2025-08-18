module Main.Browser exposing ( Model, Msg, app )

type Model
  = Model (App.Switch.Model (Loading.Model Albums.Model Msg Albums.Msg) Album.Songs.Model Msg (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg)

type Msg
  = Msg (App.Switch.Msg () Album.Album (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg)

msgEquiv : Equiv Msg (App.Switch.Msg () Album.Album (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg)
msgEquiv = { to = \(Msg m) -> m, from = Msg }

modelEquiv : Equiv (App.Switch.Model (Loading.Model Albums.Model Msg Albums.Msg) Album.Songs.Model Msg (Loading.Msg Albums.Model Msg Albums.Msg) Album.Songs.Msg) Model
modelEquiv = { to = Model, from = \(Model m) -> m }

app : App Model msgE Msg
app =
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
