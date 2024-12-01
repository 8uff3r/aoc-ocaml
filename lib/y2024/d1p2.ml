open D1p1

let main () =
  let l_ints, r_ints = rl_file (open_in file_path) ([], []) in
  let sorted_l, sorted_r = (quicksort l_ints, quicksort r_ints) in

  let sum =
    List.fold_left
      (fun l_acc l ->
        let rec aux r_acc lst =
          match lst with
          | [] -> r_acc
          | hd :: tl -> (
              match hd - l with
              | less when less < 0 -> aux r_acc tl
              | 0 -> aux (r_acc + hd) tl
              | _ -> r_acc)
        in

        l_acc + aux 0 sorted_r)
      0 sorted_l
  in
  print_endline (string_of_int sum)
