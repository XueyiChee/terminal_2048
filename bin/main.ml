open! Base
open! Stdio
open! Terminal_2048

let rec loop (game:Game.t) =
  printf "Score: %d\n" (Game.score game);
  print_string (Render.board (Game.board game));
  match Game.status game with
  | Terminated -> print_endline "Terminated!"
  | In_progress -> 
  print_string "$ ";
    Out_channel.flush Out_channel.stdout;
    match In_channel.input_line In_channel.stdin with
    | None ->
      print_endline "No command passed, try again";
      loop game
    | Some raw_str ->
      match Command.of_string raw_str with
      | None ->
        print_endline "Invalid command, try again";
        loop game
      | Some cmd ->
        match cmd with
        | Quit ->
          print_endline "Quitting"
        | Direction d ->
          let game = Game.apply game (Direction d) in
          loop game

let () = 
  let game = Game.create ~rows:4 ~cols:4 () in
  loop game

