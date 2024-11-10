let file_path = "input/2023/1"

module StringMap = Map.Make (String)

(* A custom fold function that also gives the index to the callback function *)
let fold_left_string_with_index f initial s =
  let len = String.length s in
  let rec aux acc index =
    if index >= len then acc
    else
      let char = String.get s index in
      aux (f index char acc) (index + 1)
  in
  aux initial 0

let num_strs =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

(* Access the first element *)
let get_tuple_first (x, _) = x

(* Access the second element *)
let get_tuple_second (_, y) = y

let find_all_num_str line str =
  let rec aux start acc =
    try
      let index = Str.search_forward (Str.regexp_string str) line start in
      aux (index + 1) (index :: acc)
    with Not_found -> acc
  in
  aux 0 []

let find_num_strs_in_line (line : string) =
  (* acc is a tuple of (position, number) *)
  List.fold_left
    (fun acc m ->
      let found_list = find_all_num_str line (get_tuple_first m) in
      let lst =
        List.map (fun (i : int) -> (i, get_tuple_second m)) found_list
      in
      List.merge (fun _ _ -> 0) acc lst)
    [] num_strs

let find_num_in_line (line : string) =
  fold_left_string_with_index
    (fun i c acc ->
      match int_of_string_opt (Printf.sprintf "%c" c) with
      | Some n -> (i, n) :: acc
      | None -> acc)
    [] line

let find_first_last_num_in_string line =
  let found_num_strs = find_num_strs_in_line line in
  let found_nums = find_num_in_line line in
  let sorter a b = get_tuple_first a - get_tuple_first b in
  let all_nums =
    List.merge sorter
      (List.fast_sort sorter found_num_strs)
      (List.fast_sort sorter found_nums)
  in
  print_endline "ALL_NUMS: ";
  List.iter
    (fun x ->
      Printf.printf "position: %d, value: %d " (get_tuple_first x)
        (get_tuple_second x))
    all_nums;
  print_newline ();
  ( get_tuple_second (List.nth all_nums 0),
    get_tuple_second (List.nth all_nums (List.length all_nums - 1)) )

let main () =
  let ic = open_in file_path in
  let sum = ref 0 in
  (try
     while true do
       let line = input_line ic in
       print_newline ();
       print_endline line;
       match find_first_last_num_in_string line with
       | i, j ->
           sum := !sum + (10 * i) + j;
           print_endline
             ("first: " ^ string_of_int i ^ ", last: " ^ string_of_int j
            ^ ", num: "
             ^ string_of_int ((10 * i) + j))
     done
   with End_of_file -> print_endline "end of file");
  print_newline ();
  print_endline ("Total Sum: " ^ string_of_int !sum)
