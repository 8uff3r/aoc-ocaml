let file_path = "input/2024/3"

let find_all_regex_num regex line is_dont_last =
  let r = Str.regexp regex in
  let rec aux acc start dont =
    match Str.search_forward r line start with
    | _ -> (
        match Str.matched_string line with
        | "don't()" -> aux acc (start + 7) true
        | "do()" -> aux acc (start + 4) false
        | s -> (
            if dont then aux acc (start + String.length s) dont
            else
              try
                let first = int_of_string (Str.matched_group 1 line) in
                let second = int_of_string (Str.matched_group 2 line) in
                aux (acc + (first * second)) (Str.match_end ()) dont
              with _ -> (acc, dont)))
    | exception Not_found -> (acc, dont)
  in
  aux 0 0 is_dont_last

let num_regex_in_file in_channel =
  let rec aux acc is_dont_last =
    match input_line in_channel with
    | line ->
        let num_in_line, curr_dont =
          find_all_regex_num
            {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))\|don't()\|do()|}
            line is_dont_last
        in
        aux (acc + num_in_line) curr_dont
    | exception End_of_file -> acc
  in
  aux 0 false

let main () =
  let chnl = open_in file_path in
  let num_in_file = num_regex_in_file chnl in
  print_endline (string_of_int num_in_file)
