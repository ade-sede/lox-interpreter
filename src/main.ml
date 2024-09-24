let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);
  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let ic = In_channel.open_text filename in
  let results = Lexer.tokenize ic in
  let tokens = results.tokens in
  let errors = results.errors in

  let print_token t = Printf.printf "%s\n" t#to_string in
  let print_error e = Printf.eprintf "%s\n" e in

  List.iter print_error errors;
  List.iter print_token tokens;
  if List.length errors > 0 then exit 65
