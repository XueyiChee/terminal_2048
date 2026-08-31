type t =
| Direction of Direction.t 
| Quit
[@@deriving sexp]

val of_string: string -> t option
