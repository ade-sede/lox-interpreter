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

  let equal tokens =
    match (!prev_char, tokens) with
    | '=', (`EQUAL, _) :: rest ->
        (`EQUAL_EQUAL, { Tokens.lexeme = "=="; value = None }) :: rest
    | '!', (`BANG, _) :: rest ->
        (`BANG_EQUAL, { Tokens.lexeme = "!="; value = None }) :: rest
    | '<', (`LESS, _) :: rest ->
        (`LESS_EQUAL, { Tokens.lexeme = "<="; value = None }) :: rest
    | '>', (`GREATER, _) :: rest ->
        (`GREATER_EQUAL, { Tokens.lexeme = ">="; value = None }) :: rest
    | _ -> (`EQUAL, { Tokens.lexeme = "="; value = None }) :: tokens
  in

  let slash tokens =
    match (!prev_char, tokens) with
    | '/', (`SLASH, _) :: rest ->
        let _comment = In_channel.input_line ic in
        line_number := !line_number + 1;
        rest
    | _ -> (`SLASH, { Tokens.lexeme = "/"; value = None }) :: tokens
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
        Ok
          ( `STRING,
            {
              Tokens.lexeme = Printf.sprintf "\"%s\"" str;
              value = Tokens.String str;
            } )
  in

  let input_number () =
    let decimal_point = ref false in

    let rec read_number charlist =
      match In_channel.input_char ic with
      | None -> charlist
      | Some '.' when !decimal_point ->
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
    ( `NUMBER,
      {
        Tokens.lexeme = digits_str;
        value = Tokens.Number (float_of_string digits_str);
      } )
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
    | "and" -> (`AND, { Tokens.lexeme = "and"; value = None })
    | "class" -> (`CLASS, { Tokens.lexeme = "class"; value = None })
    | "else" -> (`ELSE, { Tokens.lexeme = "else"; value = None })
    | "false" -> (`FALSE, { Tokens.lexeme = "false"; value = None })
    | "for" -> (`FOR, { Tokens.lexeme = "for"; value = None })
    | "fun" -> (`FUN, { Tokens.lexeme = "fun"; value = None })
    | "if" -> (`IF, { Tokens.lexeme = "if"; value = None })
    | "nil" -> (`NIL, { Tokens.lexeme = "nil"; value = None })
    | "or" -> (`OR, { Tokens.lexeme = "or"; value = None })
    | "print" -> (`PRINT, { Tokens.lexeme = "print"; value = None })
    | "return" -> (`RETURN, { Tokens.lexeme = "return"; value = None })
    | "super" -> (`SUPER, { Tokens.lexeme = "super"; value = None })
    | "this" -> (`THIS, { Tokens.lexeme = "this"; value = None })
    | "true" -> (`TRUE, { Tokens.lexeme = "true"; value = None })
    | "var" -> (`VAR, { Tokens.lexeme = "var"; value = None })
    | "while" -> (`WHILE, { Tokens.lexeme = "while"; value = None })
    | id -> (`IDENTIFIER, { Tokens.lexeme = id; value = None })
  in

  let rec tokenize' (tokens : Tokens.token list) =
    match In_channel.input_char ic with
    | Some char ->
        let tokens =
          match char with
          | '\t' | ' ' -> tokens
          | '\n' ->
              line_number := !line_number + 1;
              tokens
          | '(' ->
              (`LEFT_PAREN, { Tokens.lexeme = "("; value = None }) :: tokens
          | ')' ->
              (`RIGHT_PAREN, { Tokens.lexeme = ")"; value = None }) :: tokens
          | '{' ->
              (`LEFT_BRACE, { Tokens.lexeme = "{"; value = None }) :: tokens
          | '}' ->
              (`RIGHT_BRACE, { Tokens.lexeme = "}"; value = None }) :: tokens
          | '.' -> (`DOT, { Tokens.lexeme = "."; value = None }) :: tokens
          | ',' -> (`COMMA, { Tokens.lexeme = ","; value = None }) :: tokens
          | ';' -> (`SEMICOLON, { Tokens.lexeme = ";"; value = None }) :: tokens
          | '*' -> (`STAR, { Tokens.lexeme = "*"; value = None }) :: tokens
          | '-' -> (`MINUS, { Tokens.lexeme = "-"; value = None }) :: tokens
          | '+' -> (`PLUS, { Tokens.lexeme = "+"; value = None }) :: tokens
          | '>' -> (`GREATER, { Tokens.lexeme = ">"; value = None }) :: tokens
          | '<' -> (`LESS, { Tokens.lexeme = "<"; value = None }) :: tokens
          | '!' -> (`BANG, { Tokens.lexeme = "!"; value = None }) :: tokens
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
          tokens =
            List.rev ((`EOF, { Tokens.lexeme = ""; value = None }) :: tokens);
          errors = List.rev !errors;
        }
  in

  tokenize' []
