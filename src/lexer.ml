type tokenize_result = { tokens : Tokens.token list; errors : string list }

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '_' -> true
  | _ -> false

let tokenize (ic : in_channel) : tokenize_result =
  let line_number = ref 1 in
  let errors = ref [] in
  let prev_char = ref ' ' in

  let rollback_ic n =
    let pos = In_channel.pos ic in
    In_channel.seek ic (Int64.sub pos n)
  in

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
  let equal tokens =
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

  let slash tokens =
    match (!prev_char, tokens) with
    | '/', head :: rest when head#token_type = Tokens.SLASH ->
        let _comment = In_channel.input_line ic in
        line_number := !line_number + 1;
        rest
    | _ -> new Tokens.slash :: tokens
  in

  let string_literal () =
    let start_line = !line_number in

    let rec read_charlist charlist =
      match In_channel.input_char ic with
      | None ->
          Result.Error
            (Printf.sprintf "[line %d] Error: Unterminated string." start_line)
      | Some '"' -> Ok (List.rev charlist)
      | Some '\n' ->
          line_number := !line_number + 1;
          read_charlist ('\n' :: charlist)
      | Some char -> read_charlist (char :: charlist)
    in

    match read_charlist [] with
    | Error e -> Error e
    | Ok charlist ->
        let str = String.of_seq (List.to_seq charlist) in
        let new_token = new Tokens.string_value str in
        Ok new_token
  in

  let input_number () =
    let decimal_point = ref false in

    let rec read_number charlist =
      match In_channel.input_char ic with
      | None -> charlist
      | Some '.' when !decimal_point ->
          (* There can be a maximum of 1 `.` in a number *)
          rollback_ic 1L;
          charlist
      | Some '.' when not !decimal_point ->
          decimal_point := true;
          read_number ('.' :: charlist)
      | Some char when is_digit char -> read_number (char :: charlist)
      | _ ->
          rollback_ic 1L;
          charlist
    in

    let charlist = read_number [] in
    let digits_str = String.of_seq (List.to_seq (List.rev charlist)) in

    new Tokens.number digits_str
  in

  let input_identifier_or_keyword () =
    let rec read_identifier_or_keyword charlist =
      match In_channel.input_char ic with
      | None -> String.of_seq (List.to_seq (List.rev charlist))
      | Some char when is_alpha char || is_digit char ->
          read_identifier_or_keyword (char :: charlist)
      | Some _ ->
          rollback_ic 1L;
          String.of_seq (List.to_seq (List.rev charlist))
    in

    match read_identifier_or_keyword [] with
    | "and" -> new Tokens.and_keyword
    | "class" -> new Tokens.class_keyword
    | "else" -> new Tokens.else_keyword
    | "false" -> new Tokens.false_keyword
    | "for" -> new Tokens.for_keyword
    | "fun" -> new Tokens.fun_keyword
    | "if" -> new Tokens.if_keyword
    | "nil" -> new Tokens.nil_keyword
    | "or" -> new Tokens.or_keyword
    | "print" -> new Tokens.print_keyword
    | "return" -> new Tokens.return_keyword
    | "super" -> new Tokens.super_keyword
    | "this" -> new Tokens.this_keyword
    | "true" -> new Tokens.true_keyword
    | "var" -> new Tokens.var_keyword
    | "while" -> new Tokens.while_keyword
    | id -> new Tokens.identifier id
  in

  let rec tokenize' (tokens : Tokens.token list) =
    match In_channel.input_char ic with
    | Some char ->
        let tokens : Tokens.token list =
          match char with
          | '\t' | ' ' -> tokens
          | '\n' ->
              line_number := !line_number + 1;
              tokens
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
          | '>' -> new Tokens.greater :: tokens
          | '<' -> new Tokens.less :: tokens
          | '!' -> new Tokens.bang :: tokens
          | '/' -> slash tokens
          | '=' -> equal tokens
          | '"' -> (
              match string_literal () with
              | Error e ->
                  errors := e :: !errors;
                  tokens
              | Ok new_token -> new_token :: tokens)
          | char when is_digit char ->
              rollback_ic 1L;
              let new_token = input_number () in
              new_token :: tokens
          | char when is_alpha char ->
              rollback_ic 1L;
              let new_token = input_identifier_or_keyword () in
              new_token :: tokens
          | unknown_char ->
              errors :=
                Printf.sprintf "[line %d] Error: Unexpected character: %c"
                  !line_number unknown_char
                :: !errors;

              tokens
        in

        (* Need to be able to look back on prev char when current char is `=` or `/` *)
        (* The current impl of the lexer does not guarantee that this variable is up to date at all points *)
        (* It works good enough for our current needs and I am satisfied with that *)
        (* A sizeable improvement would be a buffered reader such that we can reliably check previous reads without having to cache each of them manually *)
        prev_char := char;
        tokenize' tokens
    | None ->
        In_channel.close ic;
        {
          tokens = List.rev (new Tokens.eof :: tokens);
          errors = List.rev !errors;
        }
  in

  tokenize' []
