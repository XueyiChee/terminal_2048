type t = 
| Up
| Left
| Down
| Right
[@@deriving sexp]

val of_string: string -> t option