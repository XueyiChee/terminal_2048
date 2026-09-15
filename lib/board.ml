open! Base

type t = {
  rows : int option list list
}

let create ~rows ~cols =
  let rows = 
    List.init rows ~f: (fun _ -> 
      List.init cols ~f: (fun _ -> None)  
    )
  in
  {rows}

let get t ~row ~col =
  let row_list = List.nth_exn t.rows row in
  List.nth_exn row_list col 

let move _t _direction = failwith "todo"

let has_moves _t = failwith "todo"

let empty_cells _t = failwith "todo"

let place _t ~row:_ ~col:_ ~value:_ = failwith "todo"

let%expect_test "create returns an empty board of the given dimensions" =
  let t = create ~rows:2 ~cols:3 in
  Stdio.print_s [%sexp (t.rows : int option list list)];
  [%expect {| ((() () ()) (() () ())) |}]

  