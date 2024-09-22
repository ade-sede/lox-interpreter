type tokenize_result = { tokens : Tokens.token list; error_count : int }

let is_digit = function '0' .. '9' -> true | _ -> false

let tokenize ic : tokenize_result =
  let line_number = ref 1 in
  let error_count = ref 0 in
  let prev_char = ref ' ' in

  (*
    The `=` character can represent its own `EQUAL` token, but in some cases it can also merge with another character to form a different token.
    Input `!=` is a single BANG_EQUAL token
    Input `==` is a single EQUAL_EQUAL token
    
    You can see this as squashing tokens
    [EQUAL;EQUAL] becomes [EQUAL_EQUAL]
    [BANG;EQUAL] becomes BANG_EQUAL
    
    There is one pitfall to this approach: garbled input.
    Consider `!@=`
    
    `!` gives us the BANG token
    `@` is discarded as it is an unknown token
    `=` gives us the EQUAL token
    
    `!@=` yields [BANG;EQUAL] and is squashed to [BANG_EQUAL], which is obviously wrong.
    Therefore, we need to consider both previous input character and previous token.
    
    Other atypical patterns to consider: `=\n=`, `===`, etc...
  *)
  let handle_equal_case tokens =
    match (!prev_char, tokens) with
    | '=', head :: rest when head#token_type = Tokens.EQUAL ->
        new Tokens.equal_equal :: rest
    | '!', head :: rest when head#token_type = Tokens.BANG ->
        new Tokens.bang_equal :: rest
    | '<', head :: rest when head#token_type = Tokens.LESS ->
        new Tokens.less_equal :: rest
    | '>', head :: rest when head#token_type = Tokens.GREATER ->
        new Tokens.greater_equal :: rest
    | _ -> new Tokens.equal :: tokens
  in

  let handle_slash_case tokens =
    match (!prev_char, tokens) with
    | '/', head :: rest when head#token_type = Tokens.SLASH ->
        let _comment = In_channel.input_line ic in
        line_number := !line_number + 1;
        rest
    | _ -> new Tokens.slash :: tokens
  in

  let input_string_literal () =
    let start_line = !line_number in

    let rec read_charlist charlist =
      match In_channel.input_char ic with
      | None ->
          error_count := !error_count + 1;
          Printf.eprintf "[line %d] Error: Unterminated string." start_line;
          None
      | Some char -> (
          prev_char := char;
          match char with
          | '"' -> Some (List.rev charlist)
          | '\n' ->
              line_number := !line_number + 1;
              read_charlist ('\n' :: charlist)
          | char -> read_charlist (char :: charlist))
    in

    match read_charlist [] with
    | None -> None
    | Some charlist -> Some (String.of_seq (List.to_seq charlist))
  in

  let rollback n =
    let pos = In_channel.pos ic in
    In_channel.seek ic (Int64.sub pos n)
  in

  let input_number () =
    let decimal_point = ref false in

    let rec read_number charlist =
      match In_channel.input_char ic with
      | None -> String.of_seq (List.to_seq (List.rev charlist))
      | Some '.' ->
          if !decimal_point == true then (
            rollback 1L;
            String.of_seq (List.to_seq (List.rev charlist)))
          else (
            decimal_point := true;
            read_number ('.' :: charlist))
      | Some char when is_digit char -> read_number (char :: charlist)
      | _ ->
          rollback 1L;
          String.of_seq (List.to_seq (List.rev charlist))
    in

    read_number []
  in

  let rec tokenize' (tokens : Tokens.token list) =
    match In_channel.input_char ic with
    | Some char ->
        let tokens : Tokens.token list =
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
          | '=' -> handle_equal_case tokens
          | '>' -> new Tokens.greater :: tokens
          | '<' -> new Tokens.less :: tokens
          | '!' -> new Tokens.bang :: tokens
          | '/' -> handle_slash_case tokens
          | '"' -> (
              match input_string_literal () with
              | None -> tokens
              | Some string ->
                  let new_token = new Tokens.string_value string in
                  new_token :: tokens)
          | '\t' | ' ' -> tokens
          | '\n' ->
              line_number := !line_number + 1;
              tokens
          | char when is_digit char -> (
              rollback 1L;
              match input_number () with
              | str ->
                  let new_token = new Tokens.number str in
                  new_token :: tokens)
          | unknown_literal ->
              error_count := !error_count + 1;

              Printf.eprintf "[line %d] Error: Unexpected character: %c\n"
                !line_number unknown_literal;

              tokens
        in

        prev_char := char;
        tokenize' tokens
    | None ->
        In_channel.close ic;
        {
          tokens = List.rev (new Tokens.eof :: tokens);
          error_count = !error_count;
        }
  in

  tokenize' []
