let file_path = "input/2023/2"

type model = (int * (int * int * int) list) list

let sample = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

module StringMap = Map.Make (String)

let print_list (lst : string list) = List.iter print_endline lst
let change_red (_r, g, b) new_r = (new_r, g, b)
let change_green (r, _g, b) new_g = (r, new_g, b)
let change_blue (r, g, _b) new_b = (r, g, new_b)

(* Function to get the first element of a tuple *)
let tuple3_first (x, _, _) = x
let tuple2_first (x, _) = x

(* Function to get the second element of a tuple *)
let tuple3_second (_, y, _) = y
let tuple2_second (_, y) = y

(* Function to get the third element of a tuple *)
let tuple3_third (_, _, z) = z

let set_str_to_tuple (set : string) =
  let open Str in
  let colstrs = split (regexp_string ", ") set in
  let i =
    List.fold_left
      (fun acc c ->
        let c_and_num = split (regexp_string " ") c in
        StringMap.add (List.nth c_and_num 1)
          (int_of_string (List.nth c_and_num 0))
          acc)
      (StringMap.of_seq @@ List.to_seq [])
      colstrs
  in
  StringMap.fold
    (fun k d acc ->
      match k with
      | "blue" -> change_blue acc d
      | "red" -> change_red acc d
      | "green" -> change_green acc d
      | _ -> acc)
    i (0, 0, 0)

let tokenize (line : string) =
  let open Str in
  let g1 = split (regexp_string ": ") line in
  let game_num =
    int_of_string (List.nth (split (regexp_string " ") (List.nth g1 0)) 1)
  in
  print_endline (string_of_int game_num);
  let sets = split (regexp_string "; ") (List.nth g1 1) in
  let sets_tuple = List.map (fun s -> set_str_to_tuple s) sets in
  (game_num, sets_tuple)

let is_game_possible (game : (int * int * int) list)
    ((a_red, a_green, a_blue) : int * int * int) =
  let rec is_possible (lst : (int * int * int) list) (nth : int) =
    if List.length lst == nth then true
    else
      let r, g, b = List.nth lst nth in
      if r > a_red || g > a_green || b > a_blue then false
      else is_possible lst (nth + 1)
  in
  is_possible game 0

let main () =
  let ic = open_in file_path in
  let sum = ref 0 in
  (try
     while true do
       let line = input_line ic in
       print_newline ();
       print_endline line;
       let l_token = tokenize line in
       let p = is_game_possible (tuple2_second l_token) (12, 13, 14) in
       print_endline (string_of_bool p);
       if p then sum := !sum + tuple2_first l_token
     done
   with End_of_file -> print_endline "end of file");
  print_endline ("Total sum: " ^ string_of_int !sum);
  ()
