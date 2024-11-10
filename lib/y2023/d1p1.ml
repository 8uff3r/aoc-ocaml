
let file_path = "input/2023/1"

let rec find_first_last_num_in_string line seq first second =
  if String.length line < seq + 1 then
    if second < 0 then (first, first) else (first, second)
  else
    let s = String.get line seq in
    let i =
      match int_of_string_opt (Printf.sprintf "%c" s) with
      | Some found_number ->
          if first < 0 then
            find_first_last_num_in_string line (seq + 1) found_number second
          else find_first_last_num_in_string line (seq + 1) first found_number
      | None -> find_first_last_num_in_string line (seq + 1) first second
    in
    i

let main () =
  let ic = open_in file_path in
  let sum = ref 0 in
  (try
     while true do
       let line = input_line ic in
       print_newline ();
       print_endline line;
       match find_first_last_num_in_string line 0 (-1) (-1) with
       | i, j ->
           sum := !sum + (10 * i) + j;
           print_endline
             ("first: " ^ string_of_int i ^ ", second: " ^ string_of_int j
            ^ ", num: "
             ^ string_of_int ((10 * j) + i))
     done
   with End_of_file -> print_endline "end of file");

  print_endline ("Total Sum: " ^ string_of_int !sum);
  flush stdout;
  close_in ic
