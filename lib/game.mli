open! Base

(** Whether the game is still playable, or is over. [Terminated] covers both
    ways a game ends: the player quit, or no legal moves remain. *)
type status =
  | In_progress
  | Terminated
[@@deriving sexp]

(** The full state of a running game: a board, an accumulated score, and a
    status.

    [t] is kept abstract rather than a transparent record so that [apply] can
    be the only way to transition state: a transparent record would let
    callers construct a [t] with a [board] and [status] that disagree (e.g.
    [Terminated] paired with a board that still has moves), or build one
    without ever using [apply]'s spawn and termination logic. Accessors below give read
    access without exposing that risk. *)
type t

(** [create ?seed ~rows ~cols ()] returns a fresh, in-progress game on a board
    of the given dimensions, with two tiles spawned at random positions, as in
    standard 2048. [seed] makes those spawns, and every later spawn in the
    game, reproducible; omitted, the game draws from [Random.State.default]. *)
val create : ?seed:int -> rows:int -> cols:int -> unit -> t

val board : t -> Board.t
val score : t -> int
val status : t -> status

(** [apply t command] handles one turn: applying [command] to [t].

    - [Command.Direction d]: if the game status is not [Terminated], it applies
      the associated move with this direction to the board and updates the game
      state. If the move changes the board's tiles, then a new tile of value 2
      is added to a random empty slot. If there are no more moves after this move,
      game status will transition to [Terminated].
    - [Command.Quit]: Changes the game status to [Terminated] if previously
      [In_progress] *)
val apply : t -> Command.t -> t
