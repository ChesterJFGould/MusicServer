# QStore
QStore is a database which answers questions about a single relation on domain `Id x String x Value x Id`.
An element `(id, attr, val, tx)` being in this relation means that `id` has attribute `attr` with value `val`, and this became true in transaction `tx`.
`Value` is `String + Int + Id`, `Id` is isomorphic to `Int`, `String` UTF-8 encoded strings, and `Int` is 64 bit integers.

The database can only answer questions of the form `∃id, val, tx. (id, <string lit>, val, tx) ∈ DB`, meaning you can't ask what attributes an id has.
