let rec tokenize (ic : in_channel) (tokens' : 'a Tokens.token list option) :
    'a Tokens.token list =
  let tokens = match tokens' with None -> [] | Some tokens -> tokens in

  match In_channel.input_char ic with
  | None ->
      In_channel.close ic;
      List.rev (new Tokens.eof :: tokens)
  | Some char ->
      let tokens =
        match char with
        | '(' -> new Tokens.left_paren :: tokens
        | ')' -> new Tokens.right_paren :: tokens
        | '{' -> new Tokens.left_brace :: tokens
        | '}' -> new Tokens.right_brace :: tokens
        | '\n' | '\t' | ' ' -> tokens
        | _ -> failwith "Unsupported input"
      in
      tokenize ic (Some tokens)
