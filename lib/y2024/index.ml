module DayMap = Map.Make (String)

let days =
  DayMap.of_seq
  @@ List.to_seq
       [
         ("1p1", D1p1.main);
         ("1p2", D1p2.main);
         ("2p1", D2p1.main);
         ("2p2", D2p2.main);
         ("3p1", D3p1.main);
         ("3p2", D3p2.main);
       ]
