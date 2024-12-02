open D2p1

let is_dampenable lst =
  let rec aux index =
    if index == List.length lst then false
    else
      let filtered = List.filteri (fun i _ -> i != index) lst in
      let is_safe = are_ints_safe filtered in
      if is_safe then true else aux (index + 1)
  in
  aux 0

let rec is_file_safe in_channel acc =
  match input_line in_channel with
  | line when is_line_safe line -> is_file_safe in_channel (acc + 1)
  | line ->
      let nacc = if is_dampenable (ints_of_line line) then acc + 1 else acc in
      is_file_safe in_channel nacc
  | exception End_of_file -> acc

let main () =
  let safe_lines_nums = is_file_safe (open_in file_path) 0 in
  print_endline (string_of_int safe_lines_nums)
