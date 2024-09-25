type token_type =
  | EOF
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | IDENTIFIER
  | STRING
  | NUMBER
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE

type token_value = StringVal of string | NumberVal of float | None
type token = { typ : token_type; lexeme : string; value : token_value }

let create typ lexeme value = { typ; lexeme; value }
let create_string s = create STRING (Printf.sprintf "\"%s\"" s) (StringVal s)

let create_number s =
  let n = float_of_string s in
  create NUMBER s (NumberVal n)

let string_of_token_type = function
  | EOF -> "EOF"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"

let token_to_string token =
  let value_str =
    match token.value with
    | StringVal s -> s
    | NumberVal n ->
        let s = string_of_float n in
        if String.ends_with ~suffix:"." s then s ^ "0" else s
    | None -> "null"
  in
  Printf.sprintf "%s %s %s"
    (string_of_token_type token.typ)
    token.lexeme value_str
