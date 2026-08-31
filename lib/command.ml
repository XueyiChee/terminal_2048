open! Base

type t =
  | Direction of Direction.t
  | Quit
[@@deriving sexp]

let of_string raw_str =
  match Direction.of_string raw_str with
  | Some d -> Some (Direction d)
  | None ->
    match raw_str with
    | "q" -> Some Quit
    | _ -> None