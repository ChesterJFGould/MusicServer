port module Audio exposing (AudioCmd, run, play, pause, setSource, sequence)

import Json.Encode as Json
import List

port audioCommand : Json.Value -> Cmd msg

type AudioCmd
  = List (List AudioCmd)
  | Play
  | Pause
  | SetSource String

serialize : AudioCmd -> List (Json.Value)
serialize cmd =
  case cmd of
    (List l) -> List.concat (List.map serialize l)
    (Play) ->
      List.singleton <| Json.object
        [ ("command", Json.string "play")
        ]
    (Pause) ->
      List.singleton <| Json.object
        [ ("command", Json.string "pause")
        ]
    (SetSource url) ->
      List.singleton <| Json.object
        [ ("command", Json.string "setSource")
        , ("url", Json.string url)
        ]

run : AudioCmd -> Cmd msg
run cmd = audioCommand <| Json.list (\v -> v) <| serialize cmd

play : AudioCmd
play = Play

pause : AudioCmd
pause = Pause

setSource : String -> AudioCmd
setSource src = SetSource src

sequence : List AudioCmd -> AudioCmd
sequence l = List l
