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
  let rec read_all () =
    match In_channel.input_byte ic with
    | Some byte ->
        let _char = Char.chr byte in
        read_all ()
    | None ->
        Printf.printf "EOF  null\n";
        In_channel.close ic
  in
  read_all ()
