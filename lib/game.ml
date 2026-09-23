open! Base

(* maintain invariant that if game has no moves, then its status is terminated *)
type status =
  | In_progress
  | Terminated
[@@deriving sexp]

type t = {
  board: Board.t;
  score: int;
  status: status;
  random_state: Random.State.t
}

(** Returns the (row, col)s of min(n, number of empty spaces left in t) in a
    list *)
let random_empty_elements_n board ~random_state ~n =
  Board.empty_cells board
  |> List.permute ~random_state
  |> Fn.flip List.take n

let create ?seed ~rows ~cols () =
  let random_state = 
    match seed with
    | None -> Random.State.default
    | Some seed -> Random.State.make [| seed |]
  in
  let empty_board = Board.create ~rows ~cols in
  let board_with_starting_2s =  
    random_empty_elements_n empty_board ~random_state ~n:2
    |> List.fold ~init:empty_board ~f:(fun board (row, col) ->
        Board.place board ~row ~col ~value:2
      )
  in
  {
    board=board_with_starting_2s;
    score=0;
    status=In_progress;
    random_state
  }

let board t = 
  t.board

let score t =
  t.score

let status t =
  t.status

let apply_in_progress t command =
  match command with
  | Command.Quit -> {t with status=Terminated}
  | Direction d ->
    let post_move_board, added_score = Board.move t.board d in
    let new_score = t.score + added_score in
    let board_changed = not (Board.equal t.board post_move_board) in
    if board_changed then (
      let empty_cells = Board.empty_cells post_move_board in
      match List.random_element ~random_state:t.random_state empty_cells with
      | None -> {t with board=post_move_board; score=new_score; status=Terminated}
      | Some (r, c) ->
        let board_with_added_element = Board.place post_move_board ~row:r ~col:c ~value:2 in
        let new_status = if Board.has_moves board_with_added_element then In_progress else Terminated in
        {t with board=board_with_added_element; score=new_score; status=new_status})
    else
      t

let apply t command =
  match status t with
  | Terminated -> t
  | In_progress -> apply_in_progress t command
