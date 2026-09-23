open! Base

(* Box-drawing characters are multi-byte, so build repeated runs by
   concatenation rather than [String.make]. *)
let repeat s ~times = String.concat (List.init times ~f:(fun _ -> s))

let reset = "\027[0m"

let paint ~bg:(br, bg_, bb) ~fg:(fr, fg_, fb) text =
  Printf.sprintf "\027[48;2;%d;%d;%dm\027[38;2;%d;%d;%dm%s%s" br bg_ bb fr fg_ fb text reset

let hsv_to_rgb ~h ~s ~v =
  let c = v *. s in
  let h' = h /. 60. in
  let x = c *. (1. -. Float.abs (Float.mod_float h' 2. -. 1.)) in
  let r, g, b =
    if Float.(h' < 1.) then c, x, 0.
    else if Float.(h' < 2.) then x, c, 0.
    else if Float.(h' < 3.) then 0., c, x
    else if Float.(h' < 4.) then 0., x, c
    else if Float.(h' < 5.) then x, 0., c
    else c, 0., x
  in
  let m = v -. c in
  let channel f = Float.iround_exn ~dir:`Nearest ((f +. m) *. 255.) in
  channel r, channel g, channel b

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2)

(* Tiles climb the spectrum as they double: 2 is red, 2048 is violet. Values
   beyond 2048 stay at the violet end rather than wrapping back to red, which
   would make a 4096 tile look like a 2. *)
let tile_bg value =
  let top = 11 (* log2 2048 *) in
  let step = Int.min (log2 value) top in
  let fraction = Float.of_int (Int.max 0 (step - 1)) /. Float.of_int (top - 1) in
  hsv_to_rgb ~h:(fraction *. 285.) ~s:0.8 ~v:0.95

(* dark text on a light fill, light text on a dark one *)
let fg_for (r, g, b) =
  let luminance = ((299 * r) + (587 * g) + (114 * b)) / 1000 in
  if luminance > 140 then 20, 20, 20 else 245, 245, 245

let empty_bg = 45, 45, 50

let board ?(color = true) t =
  let num_rows = Board.num_rows t in
  let num_cols = Board.num_cols t in
  let cells =
    List.init num_rows ~f:(fun row ->
      List.init num_cols ~f:(fun col -> Board.get t ~row ~col))
  in
  let text = function
    | None -> ""
    | Some value -> Int.to_string value
  in
  let width =
    List.concat cells
    |> List.map ~f:(fun cell -> String.length (text cell))
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
    |> Int.max 4
  in
  (* one space of padding either side of the value *)
  let inner = width + 2 in
  let rule ~left ~mid ~right =
    left ^ String.concat ~sep:mid (List.init num_cols ~f:(fun _ -> repeat "─" ~times:inner)) ^ right
  in
  let render_cell cell =
    let value = text cell in
    let padded =
      " " ^ String.make (width - String.length value) ' ' ^ value ^ " "
    in
    if not color
    then padded
    else (
      match cell with
      | None -> paint ~bg:empty_bg ~fg:(120, 120, 130) padded
      | Some value ->
        let bg = tile_bg value in
        paint ~bg ~fg:(fg_for bg) padded)
  in
  let render_row cells =
    "│" ^ String.concat ~sep:"│" (List.map cells ~f:render_cell) ^ "│"
  in
  let body =
    List.map cells ~f:render_row
    |> List.intersperse ~sep:(rule ~left:"├" ~mid:"┼" ~right:"┤")
  in
  ((rule ~left:"┌" ~mid:"┬" ~right:"┐" :: body) @ [ rule ~left:"└" ~mid:"┴" ~right:"┘" ])
  |> List.map ~f:(fun line -> line ^ "\n")
  |> String.concat

let%expect_test "an empty board is a plain grid" =
  Stdio.print_string (board ~color:false (Board.create ~rows:2 ~cols:3));
  [%expect
    {|
    ┌──────┬──────┬──────┐
    │      │      │      │
    ├──────┼──────┼──────┤
    │      │      │      │
    └──────┴──────┴──────┘
    |}]

let%expect_test "cells widen to fit the largest tile, and stay aligned" =
  let t =
    Board.create ~rows:2 ~cols:2
    |> Board.place ~row:0 ~col:0 ~value:2
    |> Board.place ~row:1 ~col:1 ~value:65536
  in
  Stdio.print_string (board ~color:false t);
  [%expect
    {|
    ┌───────┬───────┐
    │     2 │       │
    ├───────┼───────┤
    │       │ 65536 │
    └───────┴───────┘
    |}]

let%expect_test "colour climbs the spectrum as tiles double" =
  List.iter [ 2; 8; 32; 128; 512; 2048; 8192 ] ~f:(fun value ->
    let r, g, b = tile_bg value in
    Stdio.printf "%5d  %3d %3d %3d\n" value r g b);
  [%expect
    {|
        2  242  48  48
        8  242 233  48
       32   68 242  48
      128   48 242 213
      512   48  87 242
     2048  194  48 242
     8192  194  48 242
    |}]
