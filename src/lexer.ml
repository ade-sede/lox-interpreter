let rec tokenize (ic : in_channel) (tokens' : 'a Tokens.token list option) :
    'a Tokens.token list =
  let tokens = match tokens' with None -> [] | Some tokens -> tokens in

  match In_channel.input_char ic with
  | None ->
      In_channel.close ic;
      List.rev (new Tokens.eof :: tokens)
  | Some char -> (
      match char with
      | '(' ->
          let tokens = new Tokens.left_paren :: tokens in
          tokenize ic (Some tokens)
      | ')' ->
          let tokens = new Tokens.right_paren :: tokens in
          tokenize ic (Some tokens)
      | '\n' | '\t' | ' ' -> tokenize ic (Some tokens)
      | _ -> failwith "Unsupported input")
