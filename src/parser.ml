type literal = { token : Tokens.literal_token }
and group = { expr : expression }
and unary = { operator : Tokens.unary_operator_token; expr : expression }

and binary = {
  operator : Tokens.binary_operator_token;
  left_expr : expression;
  right_expr : expression;
}

and expression =
  | Literal of literal
  | Group of group
  | Unary of unary
  | Binary of binary

type ast = expression

let rec string_of_ast ast =
  match ast with
  | Literal { token } -> (
      match token with
      | `TRUE, _ -> "true"
      | `FALSE, _ -> "false"
      | `NIL, _ -> "nil"
      | (`NUMBER | `STRING), token_body ->
          Tokens.string_of_token_value token_body.value)
  | Group { expr } ->
      let expr = string_of_ast expr in

      Printf.sprintf "(group %s)" expr
  | Unary { operator; expr } ->
      let expr = string_of_ast expr in
      let operator = match operator with `BANG, _ -> "!" | `MINUS, _ -> "-" in

      Printf.sprintf "(%s %s)" operator expr
  | Binary { operator; left_expr; right_expr } ->
      let operator =
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
      let left_expr = string_of_ast left_expr in
      let right_expr = string_of_ast right_expr in

      Printf.sprintf "(%s %s %s)" operator left_expr right_expr

let rec parse_expression tokens = parse_equality tokens

and parse_equality tokens =
  let rec parse_equality' left tokens =
    match tokens with
    | (((`BANG_EQUAL | `EQUAL_EQUAL), _) as op) :: tail -> (
        match parse_comparison tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            parse_equality'
              (Binary { left_expr = left; operator = op; right_expr = right })
              tail'
        | Ok (None, _) -> Error "Expected expression after equality operator")
    | _ -> Ok (Some left, tokens)
  in
  match parse_comparison tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_equality' left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_comparison tokens =
  let rec parse_comparison' left tokens =
    match tokens with
    | (((`LESS | `LESS_EQUAL | `GREATER | `GREATER_EQUAL), _) as op) :: tail
      -> (
        match parse_term tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            parse_comparison'
              (Binary { left_expr = left; operator = op; right_expr = right })
              tail'
        | Ok (None, _) -> Error "Expected expression after comparison operator")
    | _ -> Ok (Some left, tokens)
  in
  match parse_term tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_comparison' left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_term tokens =
  let rec parse_term' left tokens =
    match tokens with
    | (((`PLUS | `MINUS), _) as op) :: tail -> (
        match parse_factor tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            parse_term'
              (Binary { left_expr = left; operator = op; right_expr = right })
              tail'
        | Ok (None, _) -> Error "Expected expression after + or -")
    | _ -> Ok (Some left, tokens)
  in
  match parse_factor tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_term' left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_factor tokens =
  let rec parse_factor' left tokens =
    match tokens with
    | (((`STAR | `SLASH), _) as op) :: tail -> (
        match parse_unary tail with
        | Error e -> Error e
        | Ok (Some right, tail') ->
            parse_factor'
              (Binary { left_expr = left; operator = op; right_expr = right })
              tail'
        | Ok (None, _) -> Error "Expected expression after * or /")
    | _ -> Ok (Some left, tokens)
  in
  match parse_unary tokens with
  | Error e -> Error e
  | Ok (Some left, tail) -> parse_factor' left tail
  | Ok (None, _) -> Ok (None, tokens)

and parse_unary = function
  | (((`BANG | `MINUS), _) as operator) :: tail -> (
      match parse_unary tail with
      | Error e -> Error e
      | Ok (None, _) -> Error "Expect expression after unary operator."
      | Ok (Some expr, tail') ->
          let ast = Unary { operator; expr } in
          Ok (Some ast, tail'))
  | tokens -> parse_primary tokens

and parse_primary = function
  | (`LEFT_PAREN, _) :: tail -> (
      match parse_expression tail with
      | Error e -> Error e
      | Ok (None, _) -> Error "Expect expression in parentheses."
      | Ok (Some expr, tail') -> (
          match tail' with
          | (`RIGHT_PAREN, _) :: tail -> Ok (Some (Group { expr }), tail)
          | _ -> Error "Expect ')' after expression."))
  | (((`TRUE | `FALSE | `NIL | `STRING | `NUMBER), _) as token) :: tail ->
      Ok (Some (Literal { token }), tail)
  | (`EOF, _) :: _ as tokens -> Ok (None, tokens)
  | _ -> Error "Expect expression."

let parse tokens =
  match parse_expression tokens with
  | Error e -> Error e
  | Ok (None, _) -> Error "Expect expression."
  | Ok (Some ast, [ (`EOF, _) ]) -> Ok (Some ast)
  | Ok (_, _) -> Error "Unexpected tokens after expression."
