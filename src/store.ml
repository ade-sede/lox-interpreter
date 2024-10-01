let store = ref (Hashtbl.create 1000)

let set key value =
  match Hashtbl.find_opt !store key with
  | None -> Hashtbl.add !store key value
  | Some _ -> Hashtbl.replace !store key value
