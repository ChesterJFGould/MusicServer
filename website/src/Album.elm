module Album exposing ( Album, query )

import QStore.Core as Core
import QStore.Query as Query exposing ( Query )

type alias Album =
  { id : Core.Id
  , name : String
  }


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
