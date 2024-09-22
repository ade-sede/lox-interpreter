type token_value = Number of float | String of string | None

type token_type =
  | EOF
  | RIGHT_PAREN
  | LEFT_PAREN
  | RIGHT_BRACE
  | LEFT_BRACE
  | DOT
  | COMMA
  | SEMICOLON
  | STAR
  | PLUS
  | MINUS
  | EQUAL
  | EQUAL_EQUAL
  | BANG
  | BANG_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | SLASH
  | STRING
  | NUMBER

let string_of_token_type = function
  | EOF -> "EOF"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | LEFT_BRACE -> "LEFT_BRACE"
  | DOT -> "DOT"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | STAR -> "STAR"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | SLASH -> "SLASH"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"

class virtual token =
  object (self)
    method virtual token_type : token_type
    method virtual lexeme : string
    method virtual value : token_value

    method to_string =
      let token_type = string_of_token_type self#token_type in
      let value_str =
        match self#value with
        | None -> "null"
        | _ ->
            failwith
              "Unnable to display literal value. Literal types are expected to \
               define their own to_string method"
      in

      Printf.sprintf "%s %s %s" token_type self#lexeme value_str
  end

class eof =
  object
    inherit token
    method token_type = EOF
    method lexeme = ""
    method value = None
  end

class left_paren =
  object
    inherit token
    method token_type = LEFT_PAREN
    method lexeme = "("
    method value = None
  end

class right_paren =
  object
    inherit token
    method token_type = RIGHT_PAREN
    method lexeme = ")"
    method value = None
  end

class left_brace =
  object
    inherit token
    method token_type = LEFT_BRACE
    method lexeme = "{"
    method value = None
  end

class right_brace =
  object
    inherit token
    method token_type = RIGHT_BRACE
    method lexeme = "}"
    method value = None
  end

class dot =
  object
    inherit token
    method token_type = DOT
    method lexeme = "."
    method value = None
  end

class comma =
  object
    inherit token
    method token_type = COMMA
    method lexeme = ","
    method value = None
  end

class semicolon =
  object
    inherit token
    method token_type = SEMICOLON
    method lexeme = ";"
    method value = None
  end

class star =
  object
    inherit token
    method token_type = STAR
    method lexeme = "*"
    method value = None
  end

class minus =
  object
    inherit token
    method token_type = MINUS
    method lexeme = "-"
    method value = None
  end

class plus =
  object
    inherit token
    method token_type = PLUS
    method lexeme = "+"
    method value = None
  end

class equal =
  object
    inherit token
    method token_type = EQUAL
    method lexeme = "="
    method value = None
  end

class equal_equal =
  object
    inherit token
    method token_type = EQUAL_EQUAL
    method lexeme = "=="
    method value = None
  end

class bang =
  object
    inherit token
    method token_type = BANG
    method lexeme = "!"
    method value = None
  end

class bang_equal =
  object
    inherit token
    method token_type = BANG_EQUAL
    method lexeme = "!="
    method value = None
  end

class less =
  object
    inherit token
    method token_type = LESS
    method lexeme = "<"
    method value = None
  end

class less_equal =
  object
    inherit token
    method token_type = LESS_EQUAL
    method lexeme = "<="
    method value = None
  end

class greater =
  object
    inherit token
    method token_type = GREATER
    method lexeme = ">"
    method value = None
  end

class greater_equal =
  object
    inherit token
    method token_type = GREATER_EQUAL
    method lexeme = ">="
    method value = None
  end

class slash =
  object
    inherit token
    method token_type = SLASH
    method lexeme = "/"
    method value = None
  end

class string_value string =
  object (self)
    inherit token
    method token_type = STRING
    method lexeme = Printf.sprintf "\"%s\"" string
    method value = String string
    method! to_string = Printf.sprintf "STRING %s %s" self#lexeme string
  end

class number string =
  object (self)
    inherit token
    method token_type = NUMBER
    method value = Number (float_of_string string)
    method lexeme = string

    method! to_string =
      let n =
        match self#value with Number n -> n | _ -> failwith "Expected a float"
      in
      let fstr = Float.to_string n in
      let literal =
        if String.ends_with ~suffix:"." fstr then fstr ^ "0" else fstr
      in
      Printf.sprintf "NUMBER %s %s" self#lexeme literal
  end
