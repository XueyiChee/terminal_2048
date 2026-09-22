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
  status: status
}

let create ~rows ~cols =
  { 
    board=Board.create ~rows ~cols; 
    score=0;
    status=In_progress 
  }

let board t = 
  t.board

let score t =
  t.score

let status t =
  t.status

let move _t _dir =
  failwith "todo"

let apply _t _command = failwith "todo"
