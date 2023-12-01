let lines =
  let contents = In_channel.with_open_bin "day1.txt" In_channel.input_all in
  String.split_on_char '\n' contents

let rec row : string -> (int list) = function 
  | "" -> []
  | ch -> let remRow = row (String.sub ch 1 (String.length ch - 1)) in
          let i = int_of_char (String.get ch 0) in 
          if i >= 49 && i <= 57 then (i - 48) :: remRow else remRow

let rec letter_row : string -> (int list) = function 
  | "" -> []
  | ch -> (if String.starts_with ~prefix:"one" ch then 1 :: letter_row (String.sub ch 2 (String.length ch - 2)) 
          else if String.starts_with ~prefix:"two" ch then 2 :: letter_row (String.sub ch 2 (String.length ch - 2))
          else if String.starts_with ~prefix:"three" ch then 3 :: letter_row (String.sub ch 4 (String.length ch - 4))
          else if String.starts_with ~prefix:"four" ch then 4 :: letter_row (String.sub ch 4 (String.length ch - 4)) 
          else if String.starts_with ~prefix:"five" ch then 5 :: letter_row (String.sub ch 3 (String.length ch - 3))
          else if String.starts_with ~prefix:"six" ch then 6 :: letter_row (String.sub ch 3 (String.length ch - 3))
          else if String.starts_with ~prefix:"seven" ch then 7 :: letter_row (String.sub ch 4 (String.length ch - 4))
          else if String.starts_with ~prefix:"eight" ch then 8 :: letter_row (String.sub ch 4 (String.length ch - 4))
          else if String.starts_with ~prefix:"nine" ch then 9 :: letter_row (String.sub ch 3 (String.length ch - 3))
          else 
            let remRow = letter_row (String.sub ch 1 (String.length ch - 1)) in
            let i = int_of_char (String.get ch 0) in 
            if i >= 49 && i <= 57 then (i - 48) :: remRow else remRow)  

let rec numbers : string list -> (int list) list = function 
  | [] -> []
  | line :: lines -> row line :: numbers lines

let rec letter_numbers : string list -> (int list) list = function 
  | [] -> []
  | line :: lines -> letter_row line :: letter_numbers lines

let rec values : (int list) list -> int list = function 
  | [] -> []
  | v :: vs -> ((10 * (List.nth v 0)) + (List.nth v (List.length v - 1))) :: values vs

let rec sum : int list -> int = function 
  | [] -> 0 
  | row :: rows -> row + sum rows

let main () = 
  begin
    print_int (sum (values (numbers lines)));
    print_newline ();
    print_int (sum (values (letter_numbers lines)));
    print_newline ();
  end;;

main ()
