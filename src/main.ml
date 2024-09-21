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
  let tokens, error_count = Lexer.tokenize ic in

  List.iter (fun t -> Printf.printf "%s\n" t#to_string) tokens;
  if error_count > 0 then exit 65
