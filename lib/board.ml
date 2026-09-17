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


type rotation_direction = Clockwise | Counterclockwise

(** Reflects each of the rows **)
let reflect_rows rows =
  List.map ~f:List.rev rows

(** Returns a new board type that is rotated 90° in specified direction **)
let rotate_board_exn t direction =
  match direction with
  | Clockwise -> 
    let transposed = List.transpose_exn t.rows in
    { rows = reflect_rows transposed }
  | Counterclockwise ->
    let reflected = reflect_rows t.rows in
    { rows = List.transpose_exn reflected }
  
(** given a list of ints, merges towards the right following usual 2048 rules **)
let merge_list_right l =  
  (* first entry is whether or not recent insert is merged or not
     second entry is the resulting merged list *)
  snd (List.fold_right l ~init:(true, []) ~f:(fun x accum -> 
    let recent_merged, curr_tail = accum in
    match curr_tail with
    | [] -> (false, [x])
    | hd :: tl ->
      let should_merge = (not recent_merged) && (hd = x) in
      if should_merge then
        (true, 2 * x :: tl)
      else
        (false, x :: curr_tail)     
  ))
let move _t _direction = failwith "todo"

let has_moves _t = failwith "todo"

let empty_cells _t = failwith "todo"

let place _t ~row:_ ~col:_ ~value:_ = failwith "todo"

let%expect_test "create returns an empty board of the given dimensions" =
  let t = create ~rows:2 ~cols:3 in
  Stdio.print_s [%sexp (t.rows : int option list list)];
  [%expect {| ((() () ()) (() () ())) |}]

  (* Test helpers. Tiles are distinct and boards are non-square so that any
   mix-up between rotation, transpose, and reflection shows up in the output. *)
let of_ints rows = { rows = List.map rows ~f:(List.map ~f:Option.some) }

let print_rows rows =
  List.iter rows ~f:(fun row ->
    List.map row ~f:(function None -> "." | Some v -> Int.to_string v)
    |> String.concat ~sep:" "
    |> Stdio.print_endline)

let print t = print_rows t.rows

let%expect_test "reflect_rows reverses each row, keeps row order" =
  print_rows (reflect_rows (of_ints [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]).rows);
  [%expect {|
    3 2 1
    6 5 4
    |}]

let%expect_test "reflect_rows on empty cells and a single column" =
  print_rows (reflect_rows [ [ None; Some 2 ]; [ Some 4; None ] ]);
  [%expect {|
    2 .
    . 4
    |}];
  print_rows (reflect_rows (of_ints [ [ 1 ]; [ 2 ] ]).rows);
  [%expect {|
    1
    2
    |}]

let%expect_test "rotate_board_exn Clockwise on a 2x3 board" =
  print (rotate_board_exn (of_ints [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]) Clockwise);
  [%expect {|
    4 1
    5 2
    6 3
    |}]

let%expect_test "rotate_board_exn Counterclockwise on a 2x3 board" =
  print (rotate_board_exn (of_ints [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]) Counterclockwise);
  [%expect {|
    3 6
    2 5
    1 4
    |}]

let%test_unit "reflect_rows is an involution" =
  let rows = [ [ Some 1; None; Some 3 ]; [ None; Some 5; Some 6 ] ] in
  [%test_eq: int option list list] (reflect_rows (reflect_rows rows)) rows

let%test_unit "rotation algebra: cw∘ccw = id, and four turns = id" =
  let t = of_ints [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
  let rot d t = rotate_board_exn t d in
  let same a b = [%test_eq: int option list list] a.rows b.rows in
  same (t |> rot Clockwise |> rot Counterclockwise) t;
  same (t |> rot Counterclockwise |> rot Clockwise) t;
  same (t |> rot Clockwise |> rot Clockwise |> rot Clockwise |> rot Clockwise) t;
  same
    (t |> rot Clockwise |> rot Clockwise)
    (t |> rot Counterclockwise |> rot Counterclockwise)

let%expect_test "merge_list_right follows 2048 merge rules" =
  List.iter
    [ []
    ; [ 2 ]
    ; [ 2; 4 ]
    ; [ 2; 2 ]
    ; [ 2; 4; 2 ] (* equal but not adjacent: no merge *)
    ; [ 2; 2; 2 ] (* the rightmost pair merges first *)
    ; [ 2; 2; 2; 2 ] (* two separate merges *)
    ; [ 4; 2; 2 ] (* a merged tile does not merge again in the same move *)
    ; [ 2; 2; 4 ]
    ; [ 8; 4; 4; 8 ]
    ]
    ~f:(fun l ->
      Stdio.print_s [%sexp (l : int list), "->", (merge_list_right l : int list)]);
  [%expect {|
    (() -> ())
    ((2) -> (2))
    ((2 4) -> (2 4))
    ((2 2) -> (4))
    ((2 4 2) -> (2 4 2))
    ((2 2 2) -> (2 4))
    ((2 2 2 2) -> (4 4))
    ((4 2 2) -> (4 4))
    ((2 2 4) -> (4 4))
    ((8 4 4 8) -> (8 8 8))
    |}]

let%test_unit "merge_list_right preserves the total and never adds tiles" =
  List.iter
    [ [ 2; 2; 2 ]; [ 4; 4; 8; 8 ]; [ 2; 4; 8; 16 ]; [ 2; 2; 4; 8 ]; [ 16; 16; 16; 16 ] ]
    ~f:(fun l ->
      let merged = merge_list_right l in
      let sum = List.sum (module Int) ~f:Fn.id in
      [%test_eq: int] (sum merged) (sum l);
      assert (List.length merged <= List.length l))
