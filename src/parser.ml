type literal = { token : Tokens.literal_token }
and group = { expr : expression }
and unary = { operator : Tokens.unary_operator_token; expr : expression }

and binary = {
  operator : Tokens.binary_operator_token;
  left_expr : expression;
  right_expr : expression;
}

and assignment = { identifier : Tokens.identifier_token; expr : expression }

and expression =
  | Literal of literal
  | Group of group
  | Unary of unary
  | Binary of binary
  | Identifier of { token : Tokens.identifier_token }
  | Assignment of assignment

and printStmt = { expr : expression }
and exprStmt = { expr : expression }

and variable_declaration = {
  identifier : Tokens.identifier_token;
  initial_value : expression option;
}

and statement =
  | ExprStmt of exprStmt
  | PrintStmt of printStmt
  | Declaration of variable_declaration
  | Block of statement list

and program = statement list

let rec string_of_expr expr =
  match expr with
  | Identifier { token } -> (
      let _, body = token in
      match body.value with String s -> s | _ -> assert false)
  | Literal { token } -> (
      match token with
      | `TRUE, _ -> "true"
      | `FALSE, _ -> "false"
      | `NIL, _ -> "nil"
      | (`NUMBER | `STRING), token_body ->
          Tokens.string_of_token_value token_body.value)
  | Group { expr } -> Printf.sprintf "(group %s)" (string_of_expr expr)
  | Unary { operator; expr } ->
      let op_str = match operator with `BANG, _ -> "!" | `MINUS, _ -> "-" in
      Printf.sprintf "(%s %s)" op_str (string_of_expr expr)
  | Binary { operator; left_expr; right_expr } ->
      let op_str =
        match operator with
        | `PLUS, _ -> "+"
        | `MINUS, _ -> "-"
        | `SLASH, _ -> "/"
        | `STAR, _ -> "*"
        | `BANG_EQUAL, _ -> "!="
        | `EQUAL, _ -> "="
        | `EQUAL_EQUAL, _ -> "=="
        | `GREATER, _ -> ">"
        | `GREATER_EQUAL, _ -> ">="
        | `LESS, _ -> "<"
        | `LESS_EQUAL, _ -> "<="
      in
      Printf.sprintf "(%s %s %s)" op_str (string_of_expr left_expr)
        (string_of_expr right_expr)
  | Assignment _ ->
      failwith "String representation of 'Assignment' is not supported"

let rec parse_statement tokens =
  match tokens with
  | (`LEFT_BRACE, _) :: tail -> parse_block tail []
  | (`PRINT, _) :: tail -> parse_print_statement tail
  | (`VAR, _) :: (`IDENTIFIER, id) :: tail -> parse_var_declaration id tail
  | _ -> parse_expression_statement tokens

and parse_block tokens acc =
  match parse_statement tokens with
  | Error e -> Error e
  | Ok (None, _) -> Error "Expected statement."
  | Ok (Some new_statement, (`RIGHT_BRACE, _) :: tail') ->
      let statements = List.rev (new_statement :: acc) in
      Ok (Some (Block statements), tail')
  | Ok (Some new_statement, tail) -> parse_block tail (new_statement :: acc)

and parse_print_statement tokens =
  match parse_expression tokens with
  | Error e -> Error e
  | Ok (None, _) -> Error "Expected expression after print keyword"
  | Ok (Some expr, (`SEMICOLON, _) :: tail) ->
      Ok (Some (PrintStmt { expr }), tail)
  | Ok (Some _, _) -> Error "Expected ';' after expression"

and parse_var_declaration id = function
  | (`EQUAL, _) :: tail -> (
      match parse_expression tail with
      | Error e -> Error e
      | Ok (None, _) -> Error "Expected expression after '='"
      | Ok (Some expr, (`SEMICOLON, _) :: tail') ->
          Ok
            ( Some
                (Declaration
                   { initial_value = Some expr; identifier = (`IDENTIFIER, id) }),
              tail' )
      | Ok (Some _, _) -> Error "Expected ';' after expression")
  | (`SEMICOLON, _) :: tail ->
      Ok
        ( Some
            (Declaration
               { initial_value = None; identifier = (`IDENTIFIER, id) }),
          tail )
  | _ -> Error "Expected '=' or ';' after variable name"

and parse_expression_statement tokens =
  match parse_expression tokens with
  | Error e -> Error e
  | Ok (None, _) -> Error "Expected expression"
  | Ok (Some expr, (`SEMICOLON, _) :: tail) ->
      Ok (Some (ExprStmt { expr }), tail)
  | Ok (Some _, _) -> Error "Expected ';' after expression"

and parse_expression tokens = parse_equality tokens

and parse_equality tokens =
  let rec parse_equality_tail left = function
    | (((`BANG_EQUAL | `EQUAL_EQUAL), _) as op) :: tail -> (
        match parse_comparison tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            let new_left =
              Binary { left_expr = left; operator = op; right_expr = right }
            in
            parse_equality_tail new_left tail'
        | Ok (None, _) -> Error "Expected expression after equality operator")
    | tokens -> Ok (Some left, tokens)
  in
  match parse_comparison tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_equality_tail left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_comparison tokens =
  let rec parse_comparison_tail left = function
    | (((`LESS | `LESS_EQUAL | `GREATER | `GREATER_EQUAL), _) as op) :: tail
      -> (
        match parse_term tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            let new_left =
              Binary { left_expr = left; operator = op; right_expr = right }
            in
            parse_comparison_tail new_left tail'
        | Ok (None, _) -> Error "Expected expression after comparison operator")
    | tokens -> Ok (Some left, tokens)
  in
  match parse_term tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_comparison_tail left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_term tokens =
  let rec parse_term_tail left = function
    | (((`PLUS | `MINUS), _) as op) :: tail -> (
        match parse_factor tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            let new_left =
              Binary { left_expr = left; operator = op; right_expr = right }
            in
            parse_term_tail new_left tail'
        | Ok (None, _) -> Error "Expected expression after + or -")
    | tokens -> Ok (Some left, tokens)
  in
  match parse_factor tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_term_tail left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_factor tokens =
  let rec parse_factor_tail left = function
    | (((`STAR | `SLASH), _) as op) :: tail -> (
        match parse_unary tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            let new_left =
              Binary { left_expr = left; operator = op; right_expr = right }
            in
            parse_factor_tail new_left tail'
        | Ok (None, _) -> Error "Expected expression after * or /")
    | tokens -> Ok (Some left, tokens)
  in
  match parse_unary tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_factor_tail left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_unary = function
  | (((`BANG | `MINUS), _) as operator) :: tail -> (
      match parse_unary tail with
      | Error e -> Error e
      | Ok (None, _) -> Error "Expect expression after unary operator."
      | Ok (Some expr, tail') -> Ok (Some (Unary { operator; expr }), tail'))
  | tokens -> parse_primary tokens

and parse_primary = function
  | (`LEFT_PAREN, _) :: tail -> (
      match parse_expression tail with
      | Error e -> Error e
      | Ok (None, _) -> Error "Expect expression in parentheses."
      | Ok (Some expr, (`RIGHT_PAREN, _) :: tail) ->
          Ok (Some (Group { expr }), tail)
      | Ok (Some _, _) -> Error "Expect ')' after expression.")
  | ((`IDENTIFIER, _) as identifier) :: (`EQUAL, _) :: tail -> (
      match parse_expression tail with
      | Error e -> Error e
      | Ok (None, _) -> Error "Expect expression following assignment."
      | Ok (Some expr, tail') ->
          Ok (Some (Assignment { identifier; expr }), tail'))
  | ((`IDENTIFIER, _) as token) :: tail -> Ok (Some (Identifier { token }), tail)
  | (((`TRUE | `FALSE | `NIL | `STRING | `NUMBER), _) as token) :: tail ->
      Ok (Some (Literal { token }), tail)
  | (`EOF, _) :: _ -> Ok (None, [])
  | _ -> Error "Expect expression."

let parse tokens =
  let rec parse_program acc = function
    | [] -> Ok (List.rev acc)
    | tokens -> (
        match parse_statement tokens with
        | Error e -> Error e
        | Ok (None, _) -> Error "Expect statement."
        | Ok (Some stmt, [ (`EOF, _) ]) -> Ok (List.rev (stmt :: acc))
        | Ok (Some stmt, rest) -> parse_program (stmt :: acc) rest)
  in
  parse_program [] tokens
