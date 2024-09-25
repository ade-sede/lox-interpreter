type node_type = Literal
type ast = { typ : node_type; tokens : Tokens.token list; children : ast list }

let string_of_ast ast =
  match ast.typ with
  | Literal -> (
      match ast.tokens with
      | [ token ] -> (
          match token.typ with
          | Tokens.TRUE -> "true"
          | Tokens.FALSE -> "false"
          | Tokens.NIL -> "nil"
          | Tokens.NUMBER -> Tokens.string_of_token_value token.value
          | _ -> "Unsupported token type")
      | _ -> failwith "Expected literal node to have exactly 1 token")

let parse (tokens : Tokens.token list) : ast =
  let first = List.nth tokens 0 in
  match first.typ with
  | Tokens.TRUE | Tokens.FALSE | Tokens.NIL | Tokens.NUMBER ->
      { typ = Literal; tokens = [ first ]; children = [] }
  | _ -> failwith "Unsupported token type"
