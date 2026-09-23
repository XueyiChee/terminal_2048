open! Base

(** An immutable 2048 board: a fixed-size grid of cells, each either empty or
    holding a positive power-of-two tile value. *)
type t [@@deriving equal, sexp_of]

(** [create ~rows ~cols] returns an empty board of the given dimensions.
    Both [rows] and [cols] must be positive. *)
val create : rows:int -> cols:int -> t

(** The dimensions [t] was created with. *)
val num_rows : t -> int
val num_cols : t -> int

(** [get t ~row ~col] returns the tile value at [(row, col)], or [None] if the
    cell is empty. Raises if [(row, col)] is out of bounds. *)
val get : t -> row:int -> col:int -> int option

(** [to_string_hum t] renders [t] as one line per row, cells separated by a
    space and an empty cell written as ".". Every cell is right-aligned to the
    width of the widest tile, so the columns line up. The returned string ends
    in a newline. For the player-facing rendering, see {!Render.board}. *)
val to_string_hum : t -> string

(** [move t direction] slides and merges all tiles on [t] one step in
    [direction], as in standard 2048: adjacent equal tiles merge into a
    single tile of double the value, and each tile merges at most once per
    move. Returns the resulting board together with the score gained from
    merges in this move (0 if the move changes nothing). Does not spawn a new
    tile and does not use randomness. *)
val move : t -> Direction.t -> t * int

(** [has_moves t] is [true] iff at least one of the four directions would
    change [t] under [move] or board is empty (i.e. there is an empty cell, or two adjacent
    equal tiles somewhere). *)
val has_moves : t -> bool

(** [empty_cells t] returns the [(row, col)] coordinates of every empty cell
    on [t], in unspecified order. *)
val empty_cells : t -> (int * int) list

(** [place t ~row ~col ~value] returns a copy of [t] with [value] placed at
    [(row, col)], overwriting whatever was there. Raises if [(row, col)] is
    out of bounds or [value] is not positive. *)
val place : t -> row:int -> col:int -> value:int -> t
