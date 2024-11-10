module DayMap = Map.Make (String)

let days = DayMap.of_seq @@ List.to_seq [ ("1", D1.main); ("2", D2.main) ]
