type value = Nil | Boolean of bool | String of string | Number of float

let string_of_evaluation value =
  match value with
  | Nil -> Printf.sprintf "nil"
  | Boolean true -> Printf.sprintf "true"
  | Boolean false -> Printf.sprintf "false"
  | String s -> Printf.sprintf "%s" s
  | Number n -> Printf.sprintf "%g" n

let rec evaluate ast : value =
  match ast with
  | Parser.Literal { token } -> (
      match token with
      | `TRUE, _ -> Boolean true
      | `FALSE, _ -> Boolean false
      | `NIL, _ -> Nil
      | `NUMBER, { value; _ } -> (
          match value with
          | None -> assert false
          | String _ -> assert false
          | Number n -> Number n)
      | `STRING, { value; _ } -> (
          match value with
          | None -> assert false
          | Number _ -> assert false
          | String s -> String s))
  | Parser.Group { expr } -> evaluate expr
  | Parser.Unary { expr; operator } -> (
      let value = evaluate expr in
      match (operator, value) with
      | (`BANG, _), (Boolean true | Number _) -> Boolean false
      | (`BANG, _), (Boolean false | Nil) -> Boolean true
      | (`MINUS, _), Number n -> Number (-.n)
      | _, _ -> failwith "Type mismatch")
  | Parser.Binary { left_expr; operator; right_expr } -> (
      let left = evaluate left_expr in
      let right = evaluate right_expr in
      match (operator, left, right) with
      | (`SLASH, _), Number left, Number right -> Number (Float.div left right)
      | (`STAR, _), Number left, Number right -> Number (Float.mul left right)
      | (`MINUS, _), Number left, Number right -> Number (Float.sub left right)
      | (`PLUS, _), Number left, Number right -> Number (Float.add left right)
      | (`PLUS, _), String left, String right -> String (left ^ right)
      | (`LESS, _), Number left, Number right -> Boolean (left < right)
      | (`LESS_EQUAL, _), Number left, Number right -> Boolean (left <= right)
      | (`GREATER, _), Number left, Number right -> Boolean (left > right)
      | (`GREATER_EQUAL, _), Number left, Number right -> Boolean (left >= right)
      | (`EQUAL_EQUAL, _), Number left, Number right -> Boolean (left == right)
      | (`BANG_EQUAL, _), Number left, Number right -> Boolean (left != right)
      | (`EQUAL_EQUAL, _), String left, String right -> Boolean (left == right)
      | (`BANG_EQUAL, _), String left, String right -> Boolean (left != right)
      | _, _, _ -> failwith "Unimplemented")
