module StringMap = Map.Make (String)

let capitalize_word word =
  if word = "" then ""
  else
    Char.escaped (Char.uppercase_ascii (String.get word 0))
    ^ String.sub word 1 (String.length word - 1)

let cli_year = ref 0
let cli_day = ref 0

let speclist =
  [
    ("-year", Arg.Set_int cli_year, "Selected year");
    ("-day", Arg.Set_int cli_day, "Selected day");
  ]

let selector (parameter : int ref) name default =
  if !parameter == default then (
    print_endline ("Select the " ^ name);
    int_of_string (read_line ()))
  else (
    print_endline
      (capitalize_word name ^ " " ^ string_of_int !parameter ^ " selected");
    !parameter)

let set_fields () =
  let year = selector cli_year "year" 0 in
  let day = selector cli_day "day" 0 in
  (year, day)

let () =
  Arg.parse speclist (fun _ -> ()) "";
  let year, day = set_fields () in
  print_endline
    ("Executing the code for year: " ^ string_of_int year ^ " and day: "
   ^ string_of_int day ^ "...");
  print_newline ();
  let d = StringMap.find (string_of_int year) Aoc.Index.fList in
  let j = StringMap.find (string_of_int day) d in
  j ()
