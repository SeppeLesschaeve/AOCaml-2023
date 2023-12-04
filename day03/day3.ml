let lines =
  let contents = In_channel.with_open_bin "day3.txt" In_channel.input_all in
  String.split_on_char '\n' contents

type number = { value: int; y: int; xmin: int; xmax: int; }

type symbol = { text: char; x: int; y: int; }

type grid = { numbers: number list; symbols: symbol list; }

let fold_grid s1 s2 = { numbers = List.append s1.numbers s2.numbers; symbols = List.append s1.symbols s2.symbols }

let adj symbol number = number.xmin - 1 <= symbol.x && symbol.x <= number.xmax + 1 && abs (number.y - symbol.y) <= 1

let rec of_char_list : char list -> string = fun charList -> List.fold_right (fun c -> fun cs -> (String.make 1 c) ^ (cs)) charList ""

let rec to_chars : string -> (char list) = function 
  | "" -> []
  | ch -> (String.get ch 0) :: to_chars (String.sub ch 1 (String.length ch - 1))

let rec to_char_list : string list -> (char list) list = List.map to_chars

let parse (input: string list) : grid =
  let rec aux input num xmax y numbers symbols =
    let xmax = xmax + 1 in
    match input with
    | ('0'..'9' as i1) :: ('0'..'9' as i2) :: tl -> aux (i2 :: tl) (i1 :: num) xmax y numbers symbols
    | ('0'..'9' as i1) :: tl -> 
        (
          let value = (i1 :: num) |> List.rev |> of_char_list |> int_of_string in
          let xmin = xmax - (List.length num) in
          let new_num = {value; y; xmin; xmax} in
          aux tl [] (xmax) y (new_num :: numbers) symbols
        )
    | '.' :: tl -> aux tl num xmax y numbers symbols
    | text :: tl -> aux tl num xmax y numbers ({x = xmax; y; text} :: symbols)
    | [] -> {numbers; symbols}
  in
  let rows = List.mapi (fun y -> fun row -> aux row [] 0 y [] []) (to_char_list input) in
  List.fold_left (fold_grid) ({numbers = []; symbols = []}) rows

let p1_calc grid : int = grid.numbers |> List.filter (fun num -> grid.symbols |> List.exists (fun sym -> adj sym num)) |> List.map (fun num -> num.value) |> List.fold_left (+) 0

let p2_calc grid : int = grid.symbols |> List.filter_map 
    (
      fun sym -> 
      let adj_nums = grid.numbers |> List.filter (adj sym) in
      match sym.text, adj_nums with
        | '*', { value = val1; _} :: {value = val2; _} :: []  -> Some (val1 * val2)
        | _ -> None
    ) |> List.fold_left (+) 0

let main = 
  begin
    let grid = lines |> parse in
    print_int (p1_calc grid);
    print_newline ();
    print_int (p2_calc grid);
    print_newline ();
  end

let () = main