module Track exposing (Track)

import QStore.Core as Core
import Song exposing (Song)

type alias Track = { id : Core.Id, number : Int, song : Song }
