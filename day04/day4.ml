let lines =
  let contents = In_channel.with_open_bin "day4.txt" In_channel.input_all in
  String.split_on_char '\n' contents

let process_line : string -> (int list * int list) =
    fun line -> let line = String.sub line (String.index line ':' + 1) (String.length line - String.index line ':' - 1) in
                let wins = String.sub line 0 (String.index line '|' - 1) in
                let mine = String.sub line (String.index line '|' + 1) (String.length line - String.index line '|' - 1) in
                let wins = List.map (String.trim) (String.split_on_char ' ' (String.trim wins)) in 
                let mine = List.map (String.trim) (String.split_on_char ' ' (String.trim mine)) in 
                (List.map int_of_string (List.filter (fun s -> s <> "") wins), List.map int_of_string (List.filter (fun s -> s <> "") mine))
let intersection (l1, l2) = List.length (List.filter (fun e -> List.mem e l2) l1)

let rec calculateScore : int list -> int = function 
  | [] -> 0 
  | intersect :: intersects -> int_of_float (if intersect == 0 then 0. else 2. ** float_of_int (intersect - 1)) + calculateScore intersects

let rec sum : int list -> int = function 
  | [] -> 0 
  | row :: rows -> row + sum rows

let rec updateCounts : int list -> int -> int -> int -> int -> int -> int list = 
  fun counts startI intersects i amount len -> match counts with 
  | [] -> []
  | count :: counts -> 
    if i == len then count :: counts 
    else if i < startI then count :: updateCounts counts startI intersects (i + 1) amount len 
    else if i >= startI && i < startI + intersects then (count + amount) :: updateCounts counts startI intersects (i + 1) amount len
    else count :: counts

let rec calculateCards : (int list * int list) list -> int list -> int -> int = 
  fun startCards counts i -> match startCards with 
  | [] -> sum counts
  | pair :: pairs -> 
    let intersects = intersection pair in 
    let amount = List.nth counts i in
    calculateCards pairs (updateCounts counts (i + 1) intersects 0 amount (List.length counts)) (i + 1) 

let main = 
  begin 
    print_int (calculateScore (List.map (fun line -> (intersection (process_line line))) lines));
    print_newline ();
    let cards = List.map (process_line) lines in
    let starts = List.init (List.length cards) (fun i -> 1) in 
    print_int (calculateCards cards starts 0);
    print_newline ();
  end
let () = main
