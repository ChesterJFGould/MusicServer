module ListPos exposing ( ListPos ( .. ) )

type ListPos a = Here a | Next (ListPos a)
