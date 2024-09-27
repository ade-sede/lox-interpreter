type literal_token =
  [ `TRUE | `FALSE | `NIL | `STRING | `NUMBER ] * Tokens.token_body

type unary_token = [ `BANG | `MINUS ] * Tokens.token_body

type literal = { token : literal_token }
and group = { expr : expression }
and unary = { operator : unary_token; expr : expression }
and expression = Literal of literal | Group of group | Unary of unary

type ast = expression

let rec string_of_ast ast =
  match ast with
  | Literal { token } -> (
      match token with
      | `TRUE, _ -> "true"
      | `FALSE, _ -> "false"
      | `NIL, _ -> "nil"
      | (`NUMBER | `STRING), token' -> Tokens.string_of_token_value token'.value
      )
  | Group { expr } ->
      let expr = string_of_ast expr in

      Printf.sprintf "(group %s)" expr
  | Unary { operator; expr } ->
      let expr = string_of_ast expr in
      let op = match operator with `BANG, _ -> "!" | `MINUS, _ -> "-" in

      Printf.sprintf "(%s %s)" op expr

let rec parse_expression tokens =
  let parse_group = function
    | (`LEFT_PAREN, _) :: tail -> (
        match parse_expression tail with
        | Error e -> Error e
        | Ok (None, _) -> Ok (None, tail)
        | Ok (Some expr, tail) -> (
            match tail with
            | [] -> Error "Expect expression."
            | (`RIGHT_PAREN, _) :: tail' ->
                let ast = Group { expr } in
                Ok (Some ast, tail')
            | _ -> Error "Expect right_paren."))
    | _ -> assert false
  in

  let parse_unary = function
    | (((`BANG | `MINUS), _) as token) :: tail -> (
        match parse_expression tail with
        | Error e -> Error e
        | Ok (None, _) -> Error "Expect expression."
        | Ok (Some expr, tail') ->
            let operator = token in
            let ast = Unary { expr; operator } in

            Ok (Some ast, tail'))
    | _ -> Error "Expected unary operator."
  in

  match tokens with
  | [] -> failwith "test"
  | (`EOF, _) :: tail -> Ok (None, tail)
  | (`LEFT_PAREN, _) :: _ -> parse_group tokens
  | (((`TRUE | `FALSE | `NIL | `STRING | `NUMBER), _) as token) :: tail ->
      let ast = Literal { token } in
      Ok (Some ast, tail)
  | ((`BANG | `MINUS), _) :: _ -> parse_unary tokens
  | _ -> failwith "Unimplemented"

let parse tokens =
  match parse_expression tokens with
  | Error e -> Error e
  | Ok (None, _) -> failwith "Unimplemented"
  | Ok (Some ast, _remaining_tokens) -> Ok (Some ast)
