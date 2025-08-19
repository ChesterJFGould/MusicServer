module Song exposing (Song)

import QStore.Core as Core

type alias Song = { id : Core.Id, name : String, file : Core.FileHandle }
