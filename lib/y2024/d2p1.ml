let file_path = "input/2024/2"

let ints_of_line line =
  List.map int_of_string (Str.split (Str.regexp_string " ") line)

type inde = Undetermined | Increasing | Decreasing | Equal

let are_ints_safe line_ints =
  let rec aux lst acc last_mode =
    match lst with
    | [] -> acc
    | _ :: [] -> acc
    | first :: second :: tail -> (
        let diff = second - first in
        let mode =
          match diff with
          | less when less < 0 -> Decreasing
          | bigger when bigger > 0 -> Increasing
          | _ -> Equal
        in
        match mode with
        | Equal -> false
        | m when (last_mode != m && last_mode != Undetermined) || abs diff > 3
          ->
            false
        | _ -> aux (second :: tail) true mode)
  in

  aux line_ints true Undetermined

let is_line_safe line = are_ints_safe (ints_of_line line)

let rec is_file_safe in_channel acc =
  match input_line in_channel with
  | line when is_line_safe line -> is_file_safe in_channel (acc + 1)
  | _ -> is_file_safe in_channel acc
  | exception End_of_file -> acc

let main () =
  let safe_lines_nums = is_file_safe (open_in file_path) 0 in
  print_endline (string_of_int safe_lines_nums)
