let tokenize ic =
  let line_number = ref 1 in
  let error_count = ref 0 in

  let rec tokenize' (ic : in_channel) (tokens : 'a Tokens.token list) :
      'a Tokens.token list * int =
    match In_channel.input_char ic with
    | Some char ->
        let tokens =
          match char with
          | '(' -> new Tokens.left_paren :: tokens
          | ')' -> new Tokens.right_paren :: tokens
          | '{' -> new Tokens.left_brace :: tokens
          | '}' -> new Tokens.right_brace :: tokens
          | '.' -> new Tokens.dot :: tokens
          | ',' -> new Tokens.comma :: tokens
          | ';' -> new Tokens.semicolon :: tokens
          | '*' -> new Tokens.star :: tokens
          | '-' -> new Tokens.minus :: tokens
          | '+' -> new Tokens.plus :: tokens
          | '=' -> (
              match tokens with
              | head :: rest when head#token_type = Tokens.EQUAL ->
                  new Tokens.equal_equal :: rest
              | _ -> new Tokens.equal :: tokens)
          | '\t' | ' ' -> tokens
          | '\n' ->
              line_number := !line_number + 1;
              tokens
          | unknown_literal ->
              error_count := !error_count + 1;
              Printf.eprintf "[line %d] Error: Unexpected character: %c\n"
                !line_number unknown_literal;
              tokens
        in
        tokenize' ic tokens
    | None ->
        In_channel.close ic;
        (List.rev (new Tokens.eof :: tokens), !error_count)
  in

  tokenize' ic []
