open! Base
open! Stdio
open! Terminal_2048

let rec loop () =
  print_string "$ ";
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None ->
    print_endline "No command passed, try again";
    loop ()
  | Some raw_str ->
    match Command.of_string raw_str with
    | None ->
      print_endline "Invalid command, try again";
      loop ()
    | Some cmd ->
      match cmd with
      | Quit ->
        print_endline "Quitting"
      | _ ->
        print_string "Execute command: ";
        print_s [%sexp (cmd : Command.t)];
        loop ()


let () = 
  loop ()

