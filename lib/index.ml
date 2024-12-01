module StringMap = Map.Make (String)

let fList =
  StringMap.of_seq
  @@ List.to_seq [ ("2023", Y2023.Index.days); ("2024", Y2024.Index.days) ]
