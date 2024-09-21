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

class virtual ['t] token =
  object (self)
    method virtual token_type : token_type
    method virtual lexeme : string
    method virtual literal : 't option

    method to_string =
      let token_type = string_of_token_type self#token_type in
      let literal_str =
        match (self#literal, self#literal_to_string) with
        | None, None -> "null"
        | Some literal, Some mapping_fun -> mapping_fun literal
        | _ ->
            failwith
              "Inconsistent token definition: literal & literal_to_str mapping"
      in

      Printf.sprintf "%s %s %s" token_type self#lexeme literal_str

    method virtual literal_to_string : ('t -> string) option
  end

class eof =
  object
    inherit [unit] token
    method token_type = EOF
    method lexeme = ""
    method literal = None
    method literal_to_string = None
  end

class left_paren =
  object
    inherit [unit] token
    method token_type = LEFT_PAREN
    method lexeme = "("
    method literal = None
    method literal_to_string = None
  end

class right_paren =
  object
    inherit [unit] token
    method token_type = RIGHT_PAREN
    method lexeme = ")"
    method literal = None
    method literal_to_string = None
  end

class left_brace =
  object
    inherit [unit] token
    method token_type = LEFT_BRACE
    method lexeme = "{"
    method literal = None
    method literal_to_string = None
  end

class right_brace =
  object
    inherit [unit] token
    method token_type = RIGHT_BRACE
    method lexeme = "}"
    method literal = None
    method literal_to_string = None
  end

class dot =
  object
    inherit [unit] token
    method token_type = DOT
    method lexeme = "."
    method literal = None
    method literal_to_string = None
  end

class comma =
  object
    inherit [unit] token
    method token_type = COMMA
    method lexeme = ","
    method literal = None
    method literal_to_string = None
  end

class semicolon =
  object
    inherit [unit] token
    method token_type = SEMICOLON
    method lexeme = ";"
    method literal = None
    method literal_to_string = None
  end

class star =
  object
    inherit [unit] token
    method token_type = STAR
    method lexeme = "*"
    method literal = None
    method literal_to_string = None
  end

class minus =
  object
    inherit [unit] token
    method token_type = MINUS
    method lexeme = "-"
    method literal = None
    method literal_to_string = None
  end

class plus =
  object
    inherit [unit] token
    method token_type = PLUS
    method lexeme = "+"
    method literal = None
    method literal_to_string = None
  end

class equal =
  object
    inherit [unit] token
    method token_type = EQUAL
    method lexeme = "="
    method literal = None
    method literal_to_string = None
  end

class equal_equal =
  object
    inherit [unit] token
    method token_type = EQUAL_EQUAL
    method lexeme = "=="
    method literal = None
    method literal_to_string = None
  end
