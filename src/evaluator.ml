type value = Nil | Boolean of bool | String of string | Number of float

let evaluate ast : value =
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
  | _ -> failwith "Unimplemented"
