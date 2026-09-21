open! Base

type t = {
  num_rows: int;
  num_cols: int;
  rows : int option list list
}

let create ~rows ~cols =
  let num_rows = rows in
  let num_cols = cols in
  let rows = 
    List.init num_rows ~f: (fun _ -> 
      List.init num_cols ~f: (fun _ -> None)  
    )
  in
  {num_rows; num_cols; rows}

let get t ~row ~col =
  let row_list = List.nth_exn t.rows row in
  List.nth_exn row_list col 


type rotation_direction = Clockwise | Counterclockwise

(** Reflects each of the rows **)
let reflect_rows rows =
  List.map ~f:List.rev rows

(** Returns a new board type that is rotated 90° in specified direction **)
let rotate_board_exn t ~direction =
  match direction with
  | Clockwise -> 
    let transposed = List.transpose_exn t.rows in
    { rows = reflect_rows transposed; num_rows = t.num_cols; num_cols = t.num_rows }
  | Counterclockwise ->
    let reflected = reflect_rows t.rows in
    { rows = List.transpose_exn reflected; num_rows = t.num_cols; num_cols = t.num_rows }
  
(** given a list of ints, merges towards the right following usual 2048 rules **)
let merge_list_right l =  
  (* first entry is whether or not recent insert is merged or not
     second entry is the resulting merged list 
     third entry is the score gained from merges *)
  let fold_res = 
    List.fold_right l 
      ~init:(true, [], 0) 
      ~f:(fun x accum -> 
        let recent_merged, curr_tail, score = accum in
        match curr_tail with
        | [] -> (false, [x], score)
        | hd :: tl ->
          let should_merge = (not recent_merged) && (hd = x) in
          if should_merge then
            (true, 2 * x :: tl, 2 * x + score)
          else
            (false, x :: curr_tail, score)     
      ) in
  let _, merged_list, score = fold_res in
  (merged_list, score) 


let rec prepend_list_none_count ls ~count =
  if count = 0 then
    ls
  else
    prepend_list_none_count (None::ls) ~count:(count - 1)

let prepend_list_none_till_length ls ~length =
  let curr_length = List.length ls in
  let num_to_insert = length - curr_length in
  if num_to_insert > 0 then
    prepend_list_none_count ls ~count:num_to_insert
  else
    ls

(** merges every row towards the right, returning the new board together with
    the total score gained from merges **)
let merge_board_right t =
  let new_rows, scores =
    List.map t.rows ~f:(fun l ->
      let merged_list, score = List.filter_opt l |> merge_list_right in
      let padded_row =
        merged_list
        |> List.map ~f:Option.some
        |> prepend_list_none_till_length ~length:t.num_cols
      in
      (padded_row, score))
    |> List.unzip
  in
  ({t with rows=new_rows}, List.sum (module Int) scores ~f:Fn.id)

let move t (direction: Direction.t) =
  let pre_merge, post_merge = match direction with
  | Right -> Fn.id, Fn.id
  | Up -> rotate_board_exn ~direction: Clockwise, rotate_board_exn ~direction: Counterclockwise
  | Down -> rotate_board_exn ~direction: Counterclockwise, rotate_board_exn ~direction: Clockwise
  | Left ->  
    let flip = fun x -> rotate_board_exn (rotate_board_exn x ~direction: Clockwise) ~direction: Clockwise in
    flip, flip
  in
  let prepped_board = pre_merge t in
  let merged_board, score = merge_board_right prepped_board in
  let final_board = post_merge merged_board in
  final_board, score

let empty_cells t =
  List.mapi t.rows ~f:(fun row_ind row ->
    List.mapi row ~f:(fun col_ind element ->
      match element with
      | None -> Some (row_ind, col_ind)
      | Some _ -> None
    )
  )
  |> List.concat
  |> List.filter_map ~f:Fn.id

(* returns true if and only if ls has identical neighbouring elements *)
let rec has_common_neighbours = function
  | [] | [_] -> false
  | x::y::tl -> Option.equal Int.equal x y || has_common_neighbours (y::tl)

let any_row_has_common_neighbours t =
  List.exists t.rows ~f:has_common_neighbours

(* board has no moves if and only if the there are no empty cells and no similar neighbouring cells *)
let has_moves t = 
  not (List.is_empty (empty_cells t)) 
  || any_row_has_common_neighbours t
  || any_row_has_common_neighbours (rotate_board_exn t ~direction:Clockwise) 

let place _t ~row:_ ~col:_ ~value:_ = failwith "todo"

let%expect_test "create returns an empty board of the given dimensions" =
  let t = create ~rows:2 ~cols:3 in
  Stdio.print_s [%sexp (t.rows : int option list list)];
  [%expect {| ((() () ()) (() () ())) |}]

  (* Test helpers. Tiles are distinct and boards are non-square so that any
   mix-up between rotation, transpose, and reflection shows up in the output. *)
let of_ints rows =
  { rows = List.map rows ~f:(List.map ~f:Option.some)
  ; num_rows = List.length rows
  ; num_cols = List.length (List.hd_exn rows)
  }

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
  print (rotate_board_exn (of_ints [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]) ~direction:Clockwise);
  [%expect {|
    4 1
    5 2
    6 3
    |}]

let%expect_test "rotate_board_exn Counterclockwise on a 2x3 board" =
  print (rotate_board_exn (of_ints [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]) ~direction:Counterclockwise);
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
  let rot direction t = rotate_board_exn t ~direction in
  let same a b = [%test_eq: int option list list] a.rows b.rows in
  same (t |> rot Clockwise |> rot Counterclockwise) t;
  same (t |> rot Counterclockwise |> rot Clockwise) t;
  same (t |> rot Clockwise |> rot Clockwise |> rot Clockwise |> rot Clockwise) t;
  same
    (t |> rot Clockwise |> rot Clockwise)
    (t |> rot Counterclockwise |> rot Counterclockwise)

let%expect_test "merge_list_right follows 2048 merge rules, and scores merges" =
  List.iter
    [ []
    ; [ 2 ]
    ; [ 2; 4 ]
    ; [ 2; 2 ]
    ; [ 2; 4; 2 ] (* equal but not adjacent: no merge, no score *)
    ; [ 2; 2; 2 ] (* the rightmost pair merges first *)
    ; [ 2; 2; 2; 2 ] (* two separate merges, both scored *)
    ; [ 4; 2; 2 ] (* a merged tile does not merge again in the same move *)
    ; [ 2; 2; 4 ]
    ; [ 8; 4; 4; 8 ]
    ]
    ~f:(fun l ->
      Stdio.print_s
        [%sexp (l : int list), "->", (merge_list_right l : int list * int)]);
  [%expect {|
    (() -> (() 0))
    ((2) -> ((2) 0))
    ((2 4) -> ((2 4) 0))
    ((2 2) -> ((4) 4))
    ((2 4 2) -> ((2 4 2) 0))
    ((2 2 2) -> ((2 4) 4))
    ((2 2 2 2) -> ((4 4) 8))
    ((4 2 2) -> ((4 4) 4))
    ((2 2 4) -> ((4 4) 4))
    ((8 4 4 8) -> ((8 8 8) 8))
    |}]

let%test_unit "merge_list_right preserves the total, never adds tiles, scores \
               only what it merges" =
  List.iter
    [ [ 2; 2; 2 ]; [ 4; 4; 8; 8 ]; [ 2; 4; 8; 16 ]; [ 2; 2; 4; 8 ]; [ 16; 16; 16; 16 ] ]
    ~f:(fun l ->
      let merged, score = merge_list_right l in
      let sum = List.sum (module Int) ~f:Fn.id in
      (* merging moves tiles together, it never creates or destroys value *)
      [%test_eq: int] (sum merged) (sum l);
      (* each merge turns two tiles into one, and scores that one tile *)
      let merges = List.length l - List.length merged in
      assert (merges >= 0);
      [%test_eq: bool] (score > 0) (merges > 0);
      assert (score <= sum l))

(* Boards are written as text so the test reads like the grid it describes.
   "." is an empty cell. *)
let parse_board lines =
  let rows =
    List.map lines ~f:(fun line ->
      String.split line ~on:' '
      |> List.filter ~f:(fun s -> not (String.is_empty s))
      |> List.map ~f:(function
        | "." -> None
        | v -> Some (Int.of_string v)))
  in
  { num_rows = List.length rows
  ; num_cols = List.length (List.hd_exn rows)
  ; rows
  }

let print_move lines direction =
  let board, score = move (parse_board lines) direction in
  print board;
  Stdio.printf "score: %d\n" score

let%expect_test "move Right slides and merges towards the right" =
  print_move [ "2 2 4 ."; ". . . ."; "2 . 2 4" ] Right;
  [%expect {|
    . . 4 4
    . . . .
    . . 4 4
    score: 8
    |}]

let%expect_test "move Left slides and merges towards the left" =
  print_move [ "2 2 4 ."; ". . . ."; "2 . 2 4" ] Left;
  [%expect {|
    4 4 . .
    . . . .
    4 4 . .
    score: 8
    |}]

let%expect_test "move Up slides and merges along columns" =
  print_move [ "2 2 4 ."; ". . . ."; "2 . 2 4" ] Up;
  [%expect {|
    4 2 4 4
    . . 2 .
    . . . .
    score: 4
    |}]

let%expect_test "move Down slides and merges along columns" =
  print_move [ "2 2 4 ."; ". . . ."; "2 . 2 4" ] Down;
  [%expect {|
    . . . .
    . . 4 .
    4 2 2 4
    score: 4
    |}]

let%expect_test "a tile merges at most once per move" =
  print_move [ "4 4 4 4" ] Right;
  [%expect {|
    . . 8 8
    score: 16
    |}]

let%expect_test "a move that changes nothing scores nothing" =
  print_move [ "2 4"; "4 2" ] Left;
  [%expect {|
    2 4
    4 2
    score: 0
    |}]

let%test_unit "move preserves the sum of all tiles, in every direction" =
  let t = parse_board [ "2 2 4 ."; "8 . 8 2"; "2 . 2 4" ] in
  let sum t =
    List.concat t.rows |> List.filter_opt |> List.sum (module Int) ~f:Fn.id
  in
  List.iter [ Direction.Up; Down; Left; Right ] ~f:(fun direction ->
    let moved, score = move t direction in
    [%test_eq: int] (sum moved) (sum t);
    assert (score >= 0))

(* the mli leaves the order unspecified, so sort before printing rather than
   pinning down an order callers cannot rely on *)
let print_empty_cells t =
  empty_cells t
  |> List.sort ~compare:[%compare: int * int]
  |> List.sexp_of_t [%sexp_of: int * int]
  |> Stdio.print_s

let%expect_test "every cell of a fresh board is empty" =
  print_empty_cells (create ~rows:2 ~cols:3);
  [%expect {| ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2)) |}]

let%expect_test "a full board has no empty cells" =
  print_empty_cells (parse_board [ "2 4"; "4 2" ]);
  [%expect {| () |}]

let%expect_test "coordinates are (row, col) of each empty cell" =
  print_empty_cells (parse_board [ "2 . 4"; ". . 8" ]);
  [%expect {| ((0 1) (1 0) (1 1)) |}]

let%expect_test "a non-square board reports coordinates the right way round" =
  print_empty_cells (parse_board [ ". 2 2 2"; "4 4 4 ." ]);
  [%expect {| ((0 0) (1 3)) |}]

let%test_unit "empty_cells finds exactly the cells that get returns None for" =
  let t = parse_board [ "2 . 4 ."; ". 8 . 2"; "2 2 . ." ] in
  let empties = empty_cells t in
  (* every reported cell really is empty *)
  List.iter empties ~f:(fun (row, col) ->
    [%test_eq: int option] (get t ~row ~col) None);
  (* and no empty cell is missed *)
  [%test_eq: int]
    (List.length empties)
    (List.concat t.rows |> List.count ~f:Option.is_none)

let print_has_moves lines =
  let t = parse_board lines in
  print t;
  Stdio.printf "has_moves: %b\n" (has_moves t)

let%expect_test "an empty cell means there is always a move" =
  print_has_moves [ "2 4 2"; "4 2 ."; "2 4 2" ];
  [%expect {|
    2 4 2
    4 2 .
    2 4 2
    has_moves: true
    |}]

let%expect_test "a fresh board has moves" =
  let t = create ~rows:2 ~cols:2 in
  Stdio.printf "has_moves: %b\n" (has_moves t);
  [%expect {| has_moves: true |}]

let%expect_test "a full board with no adjacent equal tiles has no moves" =
  print_has_moves [ "2 4 2"; "4 2 4"; "2 4 2" ];
  [%expect {|
    2 4 2
    4 2 4
    2 4 2
    has_moves: false
    |}]

let%expect_test "adjacent equal tiles in a row mean a move" =
  print_has_moves [ "2 2"; "4 8" ];
  [%expect {|
    2 2
    4 8
    has_moves: true
    |}]

let%expect_test "adjacent equal tiles in a column mean a move" =
  print_has_moves [ "2 4"; "2 8" ];
  [%expect {|
    2 4
    2 8
    has_moves: true
    |}]

let%expect_test "equal tiles on a diagonal are not adjacent" =
  print_has_moves [ "2 4"; "4 2" ];
  [%expect {|
    2 4
    4 2
    has_moves: false
    |}]

let%expect_test "a single full cell has no moves" =
  print_has_moves [ "2" ];
  [%expect {|
    2
    has_moves: false
    |}]

let%test_unit "has_moves agrees with whether any direction changes the board" =
  List.iter
    [ [ "2 4 2"; "4 2 4"; "2 4 2" ] (* stuck *)
    ; [ "2 4 2"; "4 2 ."; "2 4 2" ] (* an empty cell *)
    ; [ "2 2 4"; "4 8 2"; "2 4 8" ] (* a horizontal pair *)
    ; [ "2 4 8"; "2 8 4"; "4 2 2" ] (* a vertical pair *)
    ]
    ~f:(fun lines ->
      let t = parse_board lines in
      let changes direction =
        let moved, _ = move t direction in
        not ([%compare.equal: int option list list] moved.rows t.rows)
      in
      [%test_eq: bool]
        (has_moves t)
        (List.exists [ Direction.Up; Down; Left; Right ] ~f:changes))
