open! Base

(** Whether the game is still playable, has been won (a 2048 tile exists), or
    is lost (no legal moves remain). *)
type status =
  | In_progress
  | Won
  | Lost
[@@deriving sexp]

(** The full state of a running game: a board, an accumulated score, and a
    status.

    [t] is kept abstract rather than a transparent record so that [apply] can
    be the only way to transition state: a transparent record would let
    callers construct a [t] with a [board] and [status] that disagree (e.g.
    [Lost] paired with a board that still has moves), or build one without
    ever using [apply]'s spawn/win/loss logic. Accessors below give read
    access without exposing that risk. *)
type t

(** [create ~rows ~cols] returns a fresh, in-progress game on an empty board
    of the given dimensions, with two tiles spawned at random positions, as
    in standard 2048. Uses randomness as a side effect. *)
val create : rows:int -> cols:int -> t

val board : t -> Board.t
val score : t -> int
val status : t -> status

(** [apply t command] handles one turn: applying [command] to [t].

    - [Command.Direction d]: if the game is not [In_progress], returns [t]
      unchanged. Otherwise slides/merges the board via [Board.move], adds the
      merge score to [t]'s score, and, if the move actually changed the
      board, spawns one new random tile on an empty cell (side effect). Then
      recomputes [status]: [Won] if a tile of value 2048 is now present,
      else [Lost] if [Board.has_moves] is now [false], else [In_progress]. A
      command that changes nothing (an illegal/no-op move) leaves the board,
      score, and status unchanged.
    - [Command.Quit]: returns [t] unchanged; the caller is expected to
      inspect the command itself to end the program, not [t]. *)
val apply : t -> Command.t -> t
