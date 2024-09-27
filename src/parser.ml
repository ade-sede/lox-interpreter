type literal_token =
  [ `TRUE | `FALSE | `NIL | `STRING | `NUMBER ] * Tokens.token'

type unary_token = [ `BANG | `MINUS ] * Tokens.token'

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
      let inner_expr = string_of_ast expr in
      Printf.sprintf "(group %s)" inner_expr
  | Unary { operator; expr } ->
      let inner_expr = string_of_ast expr in
      let op = match operator with `BANG, _ -> "!" | `MINUS, _ -> "-" in

      Printf.sprintf "(%s %s)" op inner_expr

let parse (tokens : Tokens.token list) : ast option =
  let rec parse' tokens : ast option * Tokens.token list =
    match tokens with
    | [] -> (None, tokens)
    | head :: tail -> (
        match head with
        | ((`TRUE | `FALSE | `NIL | `STRING | `NUMBER) as typ), token' ->
            (Some (Literal { token = (typ, token') }), tail)
        | `EOF, _ -> (None, tail)
        | `LEFT_PAREN, _ -> (
            match parse' tail with
            | None, _ -> (None, tokens)
            | Some expr, tail' -> (
                match tail' with
                | (`RIGHT_PAREN, _) :: tail'' -> (Some (Group { expr }), tail'')
                | _ -> failwith "Missing closing paren"))
        | ((`BANG | `MINUS) as typ), token' -> (
            match parse' tail with
            | None, _ -> (None, tokens)
            | Some expr, tail' ->
                (Some (Unary { expr; operator = (typ, token') }), tail'))
        | _ -> failwith "Unimplemented")
  in

  let ast, _remaining_tokens = parse' tokens in
  ast
