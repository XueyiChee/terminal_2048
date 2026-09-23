open! Base

(** Player-facing rendering of a board: a grid of bordered boxes, one per cell.

    With colour on, each tile is filled with a hue taken from a rainbow ramp
    running from red at 2 to violet at 2048 and beyond, so a tile's value is
    readable at a glance from its colour alone. Colour is written as ANSI
    truecolour escapes, which every modern terminal understands but no log
    file or test output wants: pass [~color:false] for plain box drawing. *)

(** [board ?color t] renders [t]. [color] defaults to [true]. The returned
    string ends in a newline. *)
val board : ?color:bool -> Board.t -> string
