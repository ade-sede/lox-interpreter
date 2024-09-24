(*
  Dumb and simple is good. Not like this needs to be edited ever.
  TODO: check out PPX see if its a good use-case
 *)

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
  | IDENTIFIER
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FOR
  | FUN
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
  | IDENTIFIER -> "IDENTIFIER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FOR -> "FOR"
  | FUN -> "FUN"
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

class identifier name =
  object
    inherit token
    method token_type = IDENTIFIER
    method lexeme = name
    method value = None
    method! to_string = Printf.sprintf "IDENTIFIER %s null" name
  end

class and_keyword =
  object
    inherit token
    method token_type = AND
    method lexeme = "and"
    method value = None
  end

class class_keyword =
  object
    inherit token
    method token_type = CLASS
    method lexeme = "class"
    method value = None
  end

class else_keyword =
  object
    inherit token
    method token_type = ELSE
    method lexeme = "else"
    method value = None
  end

class false_keyword =
  object
    inherit token
    method token_type = FALSE
    method lexeme = "false"
    method value = None
  end

class for_keyword =
  object
    inherit token
    method token_type = FOR
    method lexeme = "for"
    method value = None
  end

class fun_keyword =
  object
    inherit token
    method token_type = FUN
    method lexeme = "fun"
    method value = None
  end

class if_keyword =
  object
    inherit token
    method token_type = IF
    method lexeme = "if"
    method value = None
  end

class nil_keyword =
  object
    inherit token
    method token_type = NIL
    method lexeme = "nil"
    method value = None
  end

class or_keyword =
  object
    inherit token
    method token_type = OR
    method lexeme = "or"
    method value = None
  end

class print_keyword =
  object
    inherit token
    method token_type = PRINT
    method lexeme = "print"
    method value = None
  end

class return_keyword =
  object
    inherit token
    method token_type = RETURN
    method lexeme = "return"
    method value = None
  end

class super_keyword =
  object
    inherit token
    method token_type = SUPER
    method lexeme = "super"
    method value = None
  end

class this_keyword =
  object
    inherit token
    method token_type = THIS
    method lexeme = "this"
    method value = None
  end

class true_keyword =
  object
    inherit token
    method token_type = TRUE
    method lexeme = "true"
    method value = None
  end

class var_keyword =
  object
    inherit token
    method token_type = VAR
    method lexeme = "var"
    method value = None
  end

class while_keyword =
  object
    inherit token
    method token_type = WHILE
    method lexeme = "while"
    method value = None
  end
