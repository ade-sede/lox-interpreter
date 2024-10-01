exception Evaluation_error of string

type value = Nil | Boolean of bool | String of string | Number of float

let scopes : (string, value) Hashtbl.t list ref = ref [ Hashtbl.create 1000 ]

let find_variable_scope identifier =
  let rec get' stores key =
    match stores with
    | [] -> None
    | store :: other_stores -> (
        match Hashtbl.find_opt store key with
        | None -> get' other_stores key
        | Some v -> Some (store, v))
  in
  get' !scopes identifier

let string_of_expression_evaluation value =
  match value with
  | Nil -> Printf.sprintf "nil"
  | Boolean true -> Printf.sprintf "true"
  | Boolean false -> Printf.sprintf "false"
  | String s -> Printf.sprintf "%s" s
  | Number n -> Printf.sprintf "%g" n

let rec evaluate_statement : Parser.statement -> (value, string) result =
 fun stmt ->
  match stmt with
  | Parser.Block statements -> evaluate_block statements
  | Parser.PrintStmt { expr } -> evaluate_print expr
  | Parser.ExprStmt { expr } -> evaluate_expression expr
  | Parser.Declaration { identifier = _, identifier; initial_value } -> (
      try evaluate_declaration identifier initial_value
      with Evaluation_error e -> Error e)

and evaluate_expression : Parser.expression -> (value, string) result =
 fun expr ->
  match expr with
  | Parser.Assignment { expr; identifier } -> (
      let _, body = identifier in
      let identifier = body.lexeme in
      match evaluate_expression expr with
      | Error e -> Error e
      | Ok new_value -> evaluate_assignment identifier new_value)
  | Parser.Identifier { token } -> evaluate_identifier token
  | Parser.Literal { token } -> evaluate_literal token
  | Parser.Group { expr } -> evaluate_expression expr
  | Parser.Unary { expr; operator } -> evaluate_unary operator expr
  | Parser.Binary { left_expr; operator; right_expr } ->
      let left = evaluate_expression left_expr in
      let right = evaluate_expression right_expr in
      evaluate_binary operator left right

and run_program program =
  match program with
  | [] -> ()
  | head :: tail -> (
      match evaluate_statement head with
      | Error e ->
          Printf.eprintf "%s\n" e;
          exit 70
      | Ok _ -> run_program tail)

and evaluate_block statements =
  let new_store = Hashtbl.create 1000 in
  scopes := new_store :: !scopes;
  run_program statements;
  scopes := List.tl !scopes;
  Ok Nil

and evaluate_print expr =
  match evaluate_expression expr with
  | Error e -> Error e
  | Ok value ->
      let s = string_of_expression_evaluation value in
      Printf.printf "%s\n" s;
      Ok value

and evaluate_declaration identifier initial_value =
  match !scopes with
  | [] -> assert false
  | current_store :: _ ->
      let varname = identifier.lexeme in
      let initial_value =
        match initial_value with
        | None -> Nil
        | Some expr -> (
            match evaluate_expression expr with
            | Error e -> raise (Evaluation_error e)
            | Ok value -> value)
      in
      (match Hashtbl.find_opt current_store varname with
      | None -> Hashtbl.add current_store varname initial_value
      | Some _ -> Hashtbl.replace current_store varname initial_value);
      Ok initial_value

and evaluate_assignment identifier new_value =
  match find_variable_scope identifier with
  | None -> Error (Printf.sprintf "Undefined variable '%s'." identifier)
  | Some (store, _) ->
      Hashtbl.replace store identifier new_value;
      Ok new_value

and evaluate_identifier token =
  let identifier = (snd token).lexeme in
  let rec get_value stores key =
    match stores with
    | [] -> None
    | store :: other_stores -> (
        match Hashtbl.find_opt store key with
        | None -> get_value other_stores key
        | Some v -> Some v)
  in
  match get_value !scopes identifier with
  | None -> Error (Printf.sprintf "Undefined variable '%s'." identifier)
  | Some value -> Ok value

and evaluate_literal token =
  match token with
  | `TRUE, _ -> Ok (Boolean true)
  | `FALSE, _ -> Ok (Boolean false)
  | `NIL, _ -> Ok Nil
  | `NUMBER, { value = Number n; _ } -> Ok (Number n)
  | `STRING, { value = String s; _ } -> Ok (String s)
  | _ -> assert false

and evaluate_unary operator expr =
  let value = evaluate_expression expr in
  match (operator, value) with
  | _, Error e -> Error e
  | (`BANG, _), Ok (Boolean true | Number _) -> Ok (Boolean false)
  | (`BANG, _), Ok (Boolean false | Nil) -> Ok (Boolean true)
  | (`MINUS, _), Ok (Number n) -> Ok (Number (-.n))
  | (`MINUS, _), _ -> Error "Operand must be a number."
  | _, _ -> failwith "Type mismatch"

and evaluate_binary operator left right =
  match (operator, left, right) with
  | _, Error e, _ -> Error e
  | _, _, Error e -> Error e
  | (`SLASH, _), Ok (Number left), Ok (Number right) ->
      Ok (Number (Float.div left right))
  | (`STAR, _), Ok (Number left), Ok (Number right) ->
      Ok (Number (Float.mul left right))
  | (`SLASH, _), _, _ -> Error "Operands must be numbers."
  | (`STAR, _), _, _ -> Error "Operands must be numbers."
  | (`MINUS, _), Ok (Number left), Ok (Number right) ->
      Ok (Number (Float.sub left right))
  | (`PLUS, _), Ok (Number left), Ok (Number right) ->
      Ok (Number (Float.add left right))
  | (`PLUS, _), Ok (String left), Ok (String right) ->
      Ok (String (left ^ right))
  | (`MINUS, _), _, _ -> Error "Operands must be numbers."
  | (`PLUS, _), _, _ -> Error "Operands must be two numbers or two strings."
  | (`LESS, _), Ok (Number left), Ok (Number right) ->
      Ok (Boolean (left < right))
  | (`LESS_EQUAL, _), Ok (Number left), Ok (Number right) ->
      Ok (Boolean (left <= right))
  | (`GREATER, _), Ok (Number left), Ok (Number right) ->
      Ok (Boolean (left > right))
  | (`GREATER_EQUAL, _), Ok (Number left), Ok (Number right) ->
      Ok (Boolean (left >= right))
  | (`LESS, _), _, _ -> Error "Operands must be numbers."
  | (`LESS_EQUAL, _), _, _ -> Error "Operands must be numbers."
  | (`GREATER, _), _, _ -> Error "Operands must be numbers."
  | (`GREATER_EQUAL, _), _, _ -> Error "Operands must be numbers."
  | (`EQUAL_EQUAL, _), Ok (Number left), Ok (Number right) ->
      Ok (Boolean (Float.equal left right))
  | (`BANG_EQUAL, _), Ok (Number left), Ok (Number right) ->
      Ok (Boolean (not (Float.equal left right)))
  | (`EQUAL_EQUAL, _), Ok (String left), Ok (String right) ->
      Ok (Boolean (String.equal left right))
  | (`BANG_EQUAL, _), Ok (String left), Ok (String right) ->
      Ok (Boolean (not (String.equal left right)))
  | (`EQUAL_EQUAL, _), Ok (Boolean left), Ok (Boolean right) ->
      Ok (Boolean (Bool.equal left right))
  | (`BANG_EQUAL, _), Ok (Boolean left), Ok (Boolean right) ->
      Ok (Boolean (not (Bool.equal left right)))
  | _, _, _ -> Ok (Boolean false)
