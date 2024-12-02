let file_path = "input/2024/2"

let ints_of_line line =
  List.map int_of_string (Str.split (Str.regexp_string " ") line)

type inde = Undetermined | Increasing | Decreasing

let is_line_safe line =
  let line_ints = ints_of_line line in

  let rec aux lst acc last_va =
    match lst with
    | [] -> acc
    | _ :: [] -> acc
    | first :: second :: tail -> (
        let diff = second - first in
        match diff with
        | 0 -> false
        | less when less < 0 && last_va == Increasing -> false
        | bigger when bigger > 0 && last_va == Decreasing -> false
        | oor when abs oor > 3 -> false
        | safe ->
            aux (second :: tail) true
              (match safe with
              | less when less < 0 -> Decreasing
              | bigger when bigger > 0 -> Increasing
              | _ -> raise (Failure "WTF")))
  in
  aux line_ints true Undetermined

let rec is_file_safe in_channel acc =
  match input_line in_channel with
  | line when is_line_safe line -> is_file_safe in_channel (acc + 1)
  | _ -> is_file_safe in_channel acc
  | exception End_of_file -> acc

let main () =
  let safe_lines_nums = is_file_safe (open_in file_path) 0 in
  print_endline (string_of_int safe_lines_nums)
