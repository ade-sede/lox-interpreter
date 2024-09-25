let print_token t = Printf.printf "%s\n" (Tokens.token_to_string t)
let print_error e = Printf.eprintf "%s\n" e

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh [tokenize | parse] <filename>\n";
    exit 1);
  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  let ic = In_channel.open_text filename in

  match command with
  | "tokenize" ->
      let results = Lexer.tokenize ic in

      List.iter print_error results.errors;
      List.iter print_token results.tokens;

      if List.length results.errors > 0 then exit 65
  | "parse" ->
      let results = Lexer.tokenize ic in
      let _ = Parser.parse [] in

      if List.length results.errors > 0 then exit 65
  | _ ->
      In_channel.close ic;
      Printf.eprintf "Unknown command: %s\n" command;
      exit 1
