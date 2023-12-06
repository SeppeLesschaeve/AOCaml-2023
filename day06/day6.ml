let lines =
  let contents = In_channel.with_open_bin "day6.txt" In_channel.input_all in
  String.split_on_char '\n' contents

let get_score time dist = 
  let x = sqrt ((time *. time) /. 4. -. dist) in
  let lower = int_of_float (floor (time /. 2. -. x +. 1.)) in
  let upper = int_of_float (ceil (time /. 2. +. x -. 1.)) in
  upper - lower + 1

let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

let times = List.map float_of_int (parse_numbers (List.nth (String.split_on_char ':' (List.nth lines 0)) 1))
let dists = List.map float_of_int (parse_numbers (List.nth (String.split_on_char ':' (List.nth lines 1)) 1))

let rec remove_spaces : string -> string = function 
  | "" -> "" 
  | ch -> 
    let removed_space = if (String.get ch 0) == ' ' then "" else String.make 1 (String.get ch 0)
    in removed_space ^ remove_spaces (String.sub ch 1 (String.length ch - 1))
let fulltime = float_of_string (remove_spaces (List.nth (String.split_on_char ':' (List.nth lines 0)) 1))
let fulldist = float_of_string (remove_spaces (List.nth (String.split_on_char ':' (List.nth lines 1)) 1))

let scores = List.map2 get_score times dists

let () = Printf.printf "%d\n" (List.fold_left ( * ) 1 scores)
let () = Printf.printf "%d\n" (get_score fulltime fulldist)
