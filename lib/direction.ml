type t = 
| Up
| Left
| Down
| Right
[@@deriving sexp]

let of_string raw_str =
  match raw_str with
  | "w" -> Some Up
  | "a" -> Some Left
  | "s" -> Some Down
  | "d" -> Some Right
  | _ -> None