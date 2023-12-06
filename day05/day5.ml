let lines =
  let contents = In_channel.with_open_bin "day5.txt" In_channel.input_all in
  String.split_on_char '\n' contents

let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

let intersect (x,y) (a,b) = max x a, (max x (max a (min y b)))

let rec get_maps lines =
  let rec aux (acc : string list list) = function
    | [] -> List.rev acc
    | line::rest ->
      if String.trim line = "" then aux ([]::acc) rest
      else aux (((String.trim line)::(List.hd acc))::(List.tl acc)) rest
  in aux [[]] lines |> List.filter (fun x -> x <> []) |> List.map (fun x -> List.tl (List.rev x)) |> List.map (List.map parse_numbers)

let seeds = parse_numbers (List.nth (String.split_on_char ':' (List.hd lines)) 1)
let maps = get_maps (List.tl (List.tl lines))

let parse_range_mapper line = ((List.nth line 1), (List.nth line 0), (List.nth line 2))

let range_map (x,y,z) (a,b) =
  let (left, right) = intersect (x, x+z) (a,b) in
  if left >= right then [(a, b)], []
  else ([(a,left); (right, b)] |> List.filter (fun (x,y) -> x<y), [(left+y-x, right+y-x)])

let rec min_elt : int list -> 'a option = function 
  | [] -> None
  | i :: [] -> Some i 
  | i :: j :: rest -> min_elt ((min i j) :: rest) 

let get_min_location seed_ranges =
  let seeds = ref seed_ranges in
  let () = List.iter (fun (map) -> 
    let rec aux lines remaining fin = match lines with
    | [] -> remaining @ fin
    | line :: rest ->
        let (x,y) = List.fold_left (fun acc arr -> (fst acc @ fst arr, snd acc @ snd arr)) ([], []) 
        (List.map (range_map (parse_range_mapper line)) remaining) in
        aux rest x  (fin @ y)
    in seeds := aux map !seeds []) maps 
  in Option.get (min_elt (List.map fst !seeds))

let get_seed_ranges1  = List.map (fun a -> (a, a+1))
let rec get_seed_ranges2 = function
  | [] -> []
  | x::y::rest -> (x,x+y-1)::get_seed_ranges2 rest
  | _ -> failwith "invalid input"

let () = print_int (get_min_location (get_seed_ranges1 seeds)); print_newline (); print_int (get_min_location (get_seed_ranges2 seeds)); print_newline ();