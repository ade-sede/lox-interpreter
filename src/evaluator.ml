type value = Nil | Boolean of bool | String of string | Number of float

let string_of_evaluation value =
  match value with
  | Nil -> Printf.sprintf "nil"
  | Boolean true -> Printf.sprintf "true"
  | Boolean false -> Printf.sprintf "false"
  | String s -> Printf.sprintf "%s" s
  | Number n -> Printf.sprintf "%g" n

let rec evaluate ast =
  match ast with
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
  | Parser.Group { expr } -> evaluate expr
  | Parser.Unary { expr; operator } -> (
      let value = evaluate expr in
      match (operator, value) with
      | _, Error e -> Error e
      | (`BANG, _), Ok (Boolean true | Number _) -> Ok (Boolean false)
      | (`BANG, _), Ok (Boolean false | Nil) -> Ok (Boolean true)
      | (`MINUS, _), Ok (Number n) -> Ok (Number (-.n))
      | (`MINUS, _), _ -> Error "Operand must be a number."
      | _, _ -> failwith "Type mismatch")
  | Parser.Binary { left_expr; operator; right_expr } -> (
      let left = evaluate left_expr in
      let right = evaluate right_expr in
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
      | (`EQUAL_EQUAL, _), Ok (Number left), Ok (Number right) ->
          Ok (Boolean (Float.equal left right))
      | (`BANG_EQUAL, _), Ok (Number left), Ok (Number right) ->
          Ok (Boolean (not (Float.equal left right)))
      | (`EQUAL_EQUAL, _), Ok (String left), Ok (String right) ->
          Ok (Boolean (String.equal left right))
      | (`BANG_EQUAL, _), Ok (String left), Ok (String right) ->
          Ok (Boolean (not (String.equal left right)))
      | _, _, _ -> Ok (Boolean false))
