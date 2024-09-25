open Tokens

type tokenize_result = { tokens : token list; errors : string list }

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
    | '=', { typ = EQUAL; _ } :: rest ->
        Tokens.create EQUAL_EQUAL "==" None :: rest
    | '!', { typ = BANG; _ } :: rest ->
        Tokens.create BANG_EQUAL "!=" None :: rest
    | '<', { typ = LESS; _ } :: rest ->
        Tokens.create LESS_EQUAL "<=" None :: rest
    | '>', { typ = GREATER; _ } :: rest ->
        Tokens.create GREATER_EQUAL ">=" None :: rest
    | _ -> Tokens.create EQUAL "=" None :: tokens
  in

  let slash tokens =
    match (!prev_char, tokens) with
    | '/', { typ = SLASH; _ } :: rest ->
        let _comment = In_channel.input_line ic in
        line_number := !line_number + 1;
        rest
    | _ -> Tokens.create SLASH "/" None :: tokens
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
        Ok (create_string str)
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

    create_number digits_str
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
    | "and" -> Tokens.create AND "and" None
    | "class" -> Tokens.create CLASS "class" None
    | "else" -> Tokens.create ELSE "else" None
    | "false" -> Tokens.create FALSE "false" None
    | "for" -> Tokens.create FOR "for" None
    | "fun" -> Tokens.create FUN "fun" None
    | "if" -> Tokens.create IF "if" None
    | "nil" -> Tokens.create NIL "nil" None
    | "or" -> Tokens.create OR "or" None
    | "print" -> Tokens.create PRINT "print" None
    | "return" -> Tokens.create RETURN "return" None
    | "super" -> Tokens.create SUPER "super" None
    | "this" -> Tokens.create THIS "this" None
    | "true" -> Tokens.create TRUE "true" None
    | "var" -> Tokens.create VAR "var" None
    | "while" -> Tokens.create WHILE "while" None
    | id -> Tokens.create IDENTIFIER id None
  in

  let rec tokenize' (tokens : token list) =
    match In_channel.input_char ic with
    | Some char ->
        let tokens : token list =
          match char with
          | '\t' | ' ' -> tokens
          | '\n' ->
              line_number := !line_number + 1;
              tokens
          | '(' -> Tokens.create LEFT_PAREN "(" None :: tokens
          | ')' -> Tokens.create RIGHT_PAREN ")" None :: tokens
          | '{' -> Tokens.create LEFT_BRACE "{" None :: tokens
          | '}' -> Tokens.create RIGHT_BRACE "}" None :: tokens
          | '.' -> Tokens.create DOT "." None :: tokens
          | ',' -> Tokens.create COMMA "," None :: tokens
          | ';' -> Tokens.create SEMICOLON ";" None :: tokens
          | '*' -> Tokens.create STAR "*" None :: tokens
          | '-' -> Tokens.create MINUS "-" None :: tokens
          | '+' -> Tokens.create PLUS "+" None :: tokens
          | '>' -> Tokens.create GREATER ">" None :: tokens
          | '<' -> Tokens.create LESS "<" None :: tokens
          | '!' -> Tokens.create BANG "!" None :: tokens
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
          tokens = List.rev (Tokens.create EOF "" None :: tokens);
          errors = List.rev !errors;
        }
  in

  tokenize' []
