module NewApp exposing ( App )

type App model msgO msgI =
  { init : (model, Cmd (Either msgO msgI)
  , update : msgI -> model -> (model, Cmd (Either msgO msgI))
  , view : model -> Html (Either msgO msgI)
  }
