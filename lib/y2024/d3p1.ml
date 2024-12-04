let file_path = "input/2024/3"

let find_all_regex_num regex line =
  let r = Str.regexp regex in
  let rec aux acc start =
    match Str.search_forward r line start with
    | _ ->
        let first = int_of_string (Str.matched_group 1 line) in
        let second = int_of_string (Str.matched_group 2 line) in
        aux (acc + (first * second)) (Str.match_end ())
    | exception Not_found -> acc
  in
  aux 0 0

let rec num_regex_in_file in_channel acc =
  match input_line in_channel with
  | line ->
      let num_in_line =
        find_all_regex_num {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}
          line
      in
      num_regex_in_file in_channel (acc + num_in_line)
  | exception End_of_file -> acc

let main () =
  let chnl = open_in file_path in
  let num_in_file = num_regex_in_file chnl 0 in
  print_endline (string_of_int num_in_file)
