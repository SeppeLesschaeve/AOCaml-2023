let lines =
  let contents = In_channel.with_open_bin "day2.txt" In_channel.input_all in
  String.split_on_char '\n' contents

let rec colorCount : string -> int list -> int list = 
  fun colorString counts ->
    let countAndColor = String.split_on_char ' ' colorString in
    let count = int_of_string (List.nth countAndColor 1) in
    let color = List.nth countAndColor 2 in
    match color with
      | "red" -> [count; List.nth counts 1; List.nth counts 2]
      | "green" -> [List.nth counts 0; count; List.nth counts 2]
      | "blue" -> [List.nth counts 0; List.nth counts 1; count]
      | _ -> counts

let rec colorCounts : string list -> int list -> (int * int * int) = 
  fun counts count -> match counts with
    | [] -> (List.nth count 0, List.nth count 1, List.nth count 2) 
    | colorString :: colors -> colorCounts colors (colorCount colorString count)

let process_line : string -> (int * int * int) list =
  fun line -> let line = String.sub line (String.index line ':' + 1) (String.length line - String.index line ':' - 1) in
              let sets = String.split_on_char ';' line in
              let sets = List.map (fun x -> String.split_on_char ',' x) sets in
              List.map (fun cubeSet -> colorCounts cubeSet [0;0;0]) sets

let is_not_possible : (int * int * int) -> (int * int * int) -> bool = 
  fun (m1, m2, m3) (c1, c2, c3) -> c1 > m1 || c2 > m2 || c3 > m3 

let rec validate_game : (int * int * int) -> (int * int * int) list -> bool -> bool = 
  fun max_cube game possible -> match game with 
  | [] -> possible 
  | one_set :: games -> let newPossible = if (not possible || is_not_possible max_cube one_set) then false else possible
                        in validate_game max_cube games newPossible  

let rec min_cube_power : ((int * int * int) list) -> (int * int * int) -> int =
  fun colors (m1, m2, m3) -> match colors with
  | [] -> (m1 * m2 * m3)
  | (c1, c2, c3) :: colors -> min_cube_power colors (max c1 m1, max c2 m2, max c3 m3)

let main () =
  let lines = List.map process_line lines in
  let max_possible = (12, 13, 14) in
  let id_sum = ref 0 in
  let min_power_sum = ref 0 in
    List.iteri (fun idx line ->
      if validate_game max_possible line true then id_sum := !id_sum + idx + 1;
      min_power_sum := !min_power_sum + min_cube_power line (1, 1, 1)) lines;
  print_int !id_sum;
  print_newline ();
  print_int !min_power_sum;
  print_newline ()

let () = main ()
