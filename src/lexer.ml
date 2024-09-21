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
let handle_equal_case prev_char tokens =
  match (prev_char, tokens) with
  | '=', head :: rest when head#token_type = Tokens.EQUAL ->
      new Tokens.equal_equal :: rest
  | '!', head :: rest when head#token_type = Tokens.BANG ->
      new Tokens.bang_equal :: rest
  | '<', head :: rest when head#token_type = Tokens.LESS ->
      new Tokens.less_equal :: rest
  | '>', head :: rest when head#token_type = Tokens.GREATER ->
      new Tokens.greater_equal :: rest
  | _ -> new Tokens.equal :: tokens

let handle_slash_case prev_char ic tokens =
  match (prev_char, tokens) with
  | '/', head :: rest when head#token_type = Tokens.SLASH ->
      let _comment = In_channel.input_line ic in
      rest
  | _ -> new Tokens.slash :: tokens

let tokenize ic =
  let line_number = ref 1 in
  let error_count = ref 0 in
  let prev = ref ' ' in

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
          | '=' -> handle_equal_case !prev tokens
          | '>' -> new Tokens.greater :: tokens
          | '<' -> new Tokens.less :: tokens
          | '!' -> new Tokens.bang :: tokens
          | '/' -> handle_slash_case !prev ic tokens
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

        prev := char;
        tokenize' ic tokens
    | None ->
        In_channel.close ic;
        (List.rev (new Tokens.eof :: tokens), !error_count)
  in

  tokenize' ic []
