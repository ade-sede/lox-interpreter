type token_value = String of string | Number of float | None
type token_body = { lexeme : string; value : token_value }

type token_type =
  [ `EOF
  | `LEFT_PAREN
  | `RIGHT_PAREN
  | `LEFT_BRACE
  | `RIGHT_BRACE
  | `COMMA
  | `DOT
  | `MINUS
  | `PLUS
  | `SEMICOLON
  | `SLASH
  | `STAR
  | `BANG
  | `BANG_EQUAL
  | `EQUAL
  | `EQUAL_EQUAL
  | `GREATER
  | `GREATER_EQUAL
  | `LESS
  | `LESS_EQUAL
  | `IDENTIFIER
  | `STRING
  | `NUMBER
  | `AND
  | `CLASS
  | `ELSE
  | `FALSE
  | `FUN
  | `FOR
  | `IF
  | `NIL
  | `OR
  | `PRINT
  | `RETURN
  | `SUPER
  | `THIS
  | `TRUE
  | `VAR
  | `WHILE ]

type token = token_type * token_body
type literal_token = [ `TRUE | `FALSE | `NIL | `STRING | `NUMBER ] * token_body
type unary_operator_token = [ `BANG | `MINUS ] * token_body

type binary_operator_token =
  [ `PLUS
  | `MINUS
  | `SLASH
  | `STAR
  | `BANG_EQUAL
  | `EQUAL
  | `EQUAL_EQUAL
  | `GREATER
  | `GREATER_EQUAL
  | `LESS
  | `LESS_EQUAL ]
  * token_body

let token_body_of_token (token : token) =
  let _typ, body = token in
  body

let create_string s =
  (`STRING, { lexeme = Printf.sprintf "\"%s\"" s; value = String s })

let create_number s =
  let n = float_of_string s in
  (`NUMBER, { lexeme = s; value = Number n })

let string_of_token_type = function
  | `EOF -> "EOF"
  | `LEFT_PAREN -> "LEFT_PAREN"
  | `RIGHT_PAREN -> "RIGHT_PAREN"
  | `LEFT_BRACE -> "LEFT_BRACE"
  | `RIGHT_BRACE -> "RIGHT_BRACE"
  | `COMMA -> "COMMA"
  | `DOT -> "DOT"
  | `MINUS -> "MINUS"
  | `PLUS -> "PLUS"
  | `SEMICOLON -> "SEMICOLON"
  | `SLASH -> "SLASH"
  | `STAR -> "STAR"
  | `BANG -> "BANG"
  | `BANG_EQUAL -> "BANG_EQUAL"
  | `EQUAL -> "EQUAL"
  | `EQUAL_EQUAL -> "EQUAL_EQUAL"
  | `GREATER -> "GREATER"
  | `GREATER_EQUAL -> "GREATER_EQUAL"
  | `LESS -> "LESS"
  | `LESS_EQUAL -> "LESS_EQUAL"
  | `IDENTIFIER -> "IDENTIFIER"
  | `STRING -> "STRING"
  | `NUMBER -> "NUMBER"
  | `AND -> "AND"
  | `CLASS -> "CLASS"
  | `ELSE -> "ELSE"
  | `FALSE -> "FALSE"
  | `FUN -> "FUN"
  | `FOR -> "FOR"
  | `IF -> "IF"
  | `NIL -> "NIL"
  | `OR -> "OR"
  | `PRINT -> "PRINT"
  | `RETURN -> "RETURN"
  | `SUPER -> "SUPER"
  | `THIS -> "THIS"
  | `TRUE -> "TRUE"
  | `VAR -> "VAR"
  | `WHILE -> "WHILE"

let string_of_token_value = function
  | String s -> s
  | Number n ->
      let s = string_of_float n in
      if String.ends_with ~suffix:"." s then s ^ "0" else s
  | None -> "null"

let string_of_token (token_type, token) =
  Printf.sprintf "%s %s %s"
    (string_of_token_type token_type)
    token.lexeme
    (string_of_token_value token.value)
