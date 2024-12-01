let file_path = "input/2024/1"

let rec rl_file in_channel acc =
  match input_line in_channel with
  | line ->
      let line_rl = Str.split (Str.regexp_string "   ") line in
      let l_acc, r_acc = acc in
      rl_file in_channel
        ( int_of_string (List.nth line_rl 0) :: l_acc,
          int_of_string (List.nth line_rl 1) :: r_acc )
  | exception End_of_file -> acc

let rec quicksort lst =
  match lst with
  | [] -> []
  | pivot :: rest ->
      let less_than_pivot = List.filter (fun x -> x < pivot) rest in
      let greater_than_pivot = List.filter (fun x -> x >= pivot) rest in
      quicksort less_than_pivot @ [ pivot ] @ quicksort greater_than_pivot

let main () =
  let l_ints, r_ints = rl_file (open_in file_path) ([], []) in
  let sorted_l, sorted_r = (quicksort l_ints, quicksort r_ints) in
  let diff_lst =
    List.mapi (fun i v -> abs (v - List.nth sorted_r i)) sorted_l
  in
  List.iter (fun v -> print_endline (string_of_int v)) diff_lst;
  let sum = List.fold_left ( + ) 0 diff_lst in
  print_newline ();
  print_endline ("Sum of all: " ^ string_of_int sum)
