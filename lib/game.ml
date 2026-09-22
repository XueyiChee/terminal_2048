open! Base

(* TODO: remove once the module is implemented *)
[@@@warning "-27-32-69"]

type status =
  | In_progress
  | Won
  | Lost
[@@deriving sexp]

type t = {
  board: Board.t;
  score: int;
}

let create ~rows ~cols =
  { 
    board=Board.create ~rows ~cols; 
    score=0 
  }

let board t = 
  t.board

let score _t = failwith "todo"

let status _t = failwith "todo"

let move _t _dir =
  failwith "todo"

let apply _t _command = failwith "todo"
