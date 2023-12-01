let lines =
  let contents = In_channel.with_open_bin "day1.txt" In_channel.input_all in
  String.split_on_char '\n' contents

let rec get_numbers : bool -> string -> (int list) = fun version line -> match line with 
  | "" -> []
  | ch -> if not version then (
            let remRow = get_numbers version (String.sub ch 1 (String.length ch - 1)) in
            let i = int_of_char (String.get ch 0) in 
            if i >= 49 && i <= 57 then (i - 48) :: remRow else remRow)
          else (if String.starts_with ~prefix:"one"        ch then 1 :: get_numbers version (String.sub ch 2 (String.length ch - 2))
                else if String.starts_with ~prefix:"two"   ch then 2 :: get_numbers version (String.sub ch 2 (String.length ch - 2))
                else if String.starts_with ~prefix:"three" ch then 3 :: get_numbers version (String.sub ch 4 (String.length ch - 4))
                else if String.starts_with ~prefix:"four"  ch then 4 :: get_numbers version (String.sub ch 4 (String.length ch - 4))
                else if String.starts_with ~prefix:"five"  ch then 5 :: get_numbers version (String.sub ch 3 (String.length ch - 3))
                else if String.starts_with ~prefix:"six"   ch then 6 :: get_numbers version (String.sub ch 3 (String.length ch - 3))
                else if String.starts_with ~prefix:"seven" ch then 7 :: get_numbers version (String.sub ch 4 (String.length ch - 4))
                else if String.starts_with ~prefix:"eight" ch then 8 :: get_numbers version (String.sub ch 4 (String.length ch - 4))
                else if String.starts_with ~prefix:"nine"  ch then 9 :: get_numbers version (String.sub ch 3 (String.length ch - 3))
                else 
                  let remRow = get_numbers version (String.sub ch 1 (String.length ch - 1)) in
                  let i = int_of_char (String.get ch 0) in 
                  if i >= 49 && i <= 57 then (i - 48) :: remRow else remRow)

let rec numbers : string list -> (int list) list = List.map (get_numbers false)

let rec letter_numbers : string list -> (int list) list = List.map (get_numbers true)

let rec values : (int list) list -> int list = List.map (fun v -> (10 * (List.nth v 0)) + (List.nth v (List.length v - 1)))

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
