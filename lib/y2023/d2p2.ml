open D2p1

let min_rgb (lst : (int * int * int) list) =
  List.fold_left
    (fun acc i ->
      let a_r, a_g, a_b = acc in
      let r, g, b = i in

      let m_r = max r a_r in
      let m_g = max g a_g in
      let m_b = max b a_b in
      (m_r, m_g, m_b))
    (0, 0, 0) lst

let main () =
  let ic = open_in D2p1.file_path in
  let sum = ref 0 in
  (try
     while true do
       let line = input_line ic in
       print_newline ();
       print_endline line;
       let l_token = D2p1.tokenize line in
       let p = min_rgb (tuple2_second l_token) in
       let r, g, b = p in
       sum := !sum + (r * g * b)
     done
   with End_of_file -> print_endline "end of file");
  print_endline ("Total sum: " ^ string_of_int !sum);
  ()
