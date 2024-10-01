exception Evaluation_error of string

type value = Nil | Boolean of bool | String of string | Number of float

(*
   1 store = 1 scope
   When we open a block, a new scope is created by prepending a new hash table to
   the list
*)
let scopes : (string, value) Hashtbl.t list ref = ref [ Hashtbl.create 1000 ]

let get key =
  let rec get' stores key =
    match stores with
    | [] -> None
    | store :: others -> (
        match Hashtbl.find_opt store key with
        | None -> get' others key
        | Some data -> Some (store, data))
  in

  get' !scopes key

let string_of_expression_evaluation value =
  match value with
  | Nil -> Printf.sprintf "nil"
  | Boolean true -> Printf.sprintf "true"
  | Boolean false -> Printf.sprintf "false"
  | String s -> Printf.sprintf "%s" s
  | Number n -> Printf.sprintf "%g" n

let rec evaluate_statement stmt =
  match stmt with
  | Parser.PrintStmt { expr } -> (
      match evaluate_expression expr with
      | Error e -> Error e
      | Ok value ->
          let s = string_of_expression_evaluation value in
          Printf.printf "%s\n" s;
          Ok value)
  | Parser.ExprStmt { expr } -> evaluate_expression expr
  | Parser.Declaration { identifier = _, { lexeme; _ }; initial_value } -> (
      match !scopes with
      | [] -> assert false
      | current_store :: _ -> (
          try
            let varname = lexeme in

            let initial_value =
              match initial_value with
              | None -> Nil
              | Some expr -> (
                  match evaluate_expression expr with
                  | Error e -> raise (Evaluation_error e)
                  | Ok value -> value)
            in

            (* When declaring a variable if the variable exists in the current scope
               its value is overwritten. If it does not exist it is added to the
               current scope, with the specified initial value *)
            (match Hashtbl.find_opt current_store varname with
            | None -> Hashtbl.add current_store varname initial_value
            | Some _ -> Hashtbl.replace current_store varname initial_value);

            Ok initial_value
          with Evaluation_error e -> Error e))

and evaluate_expression expr =
  let rec evaluate_expression' expr =
    match expr with
    | Parser.Identifier { token } -> (
        let _, body = token in
        let identifier = body.lexeme in

        (* Go up scopes recursively until a match is found. Or not *)
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
        | Some value -> Ok value)
    | Parser.Literal { token } -> (
        match token with
        | `TRUE, _ -> Ok (Boolean true)
        | `FALSE, _ -> Ok (Boolean false)
        | `NIL, _ -> Ok Nil
        | `NUMBER, { value; _ } -> (
            match value with
            | None -> assert false
            | String _ -> assert false
            | Number n -> Ok (Number n))
        | `STRING, { value; _ } -> (
            match value with
            | None -> assert false
            | Number _ -> assert false
            | String s -> Ok (String s)))
    | Parser.Group { expr } -> evaluate_expression' expr
    | Parser.Unary { expr; operator } -> (
        let value = evaluate_expression' expr in
        match (operator, value) with
        | _, Error e -> Error e
        | (`BANG, _), Ok (Boolean true | Number _) -> Ok (Boolean false)
        | (`BANG, _), Ok (Boolean false | Nil) -> Ok (Boolean true)
        | (`MINUS, _), Ok (Number n) -> Ok (Number (-.n))
        | (`MINUS, _), _ -> Error "Operand must be a number."
        | _, _ -> failwith "Type mismatch")
    | Parser.Binary { left_expr; operator; right_expr } -> (
        let left = evaluate_expression' left_expr in
        let right = evaluate_expression' right_expr in
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
        | (`PLUS, _), _, _ ->
            Error "Operands must be two numbers or two strings."
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
        | _, _, _ -> Ok (Boolean false))
  in

  evaluate_expression' expr

and run_program program =
  let rec run' program =
    match program with
    | [] -> ()
    | head :: tail -> (
        match evaluate_statement head with
        | Error e ->
            Printf.eprintf "%s\n" e;
            exit 70
        | Ok _ -> run' tail)
  in

  run' program
