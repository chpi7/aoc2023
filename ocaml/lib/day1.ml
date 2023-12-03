open Common

let filename = "inputs/day1.txt"

let is_digit = function '0' .. '9' -> true | _ -> false

let get_nums line =
    print_string line; print_newline ();
    let f acc c = if is_digit c 
    then 
        let digit = (Char.code c) - Char.code '0' in
        digit :: acc
    else
        acc
    in
    (* Accumulate the digits in reverse order *)
    let all_digits = String.fold_left f [] line in
    (* Then extract the number *)
    let last = List.hd all_digits in
    let first = all_digits |> List.rev |> List.hd in
    first * 10 + last

let replacements = [
    ("zero", "0");
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9")
]

let split_at s i = 
    (* print_endline ("split at s = " ^ s) ; *)
(
    String.sub s 0 i,
    String.sub s i ((String.length s) - i)
)

let try_replace string written digit = 
    if String.starts_with ~prefix:written string
    then
        let remainder = match split_at string (String.length written) with (_, tl) -> tl in
        digit ^ remainder
    else
        string

let remainder s = if s == "" then ("","") else (String.sub s 0 1, String.sub s 1 ((String.length s) - 1))

let rec replace_written_nums line =
    (* print_endline ("line is " ^ line) ; *)
    if 0 == String.length line
    then 
        ""
    else
        let apply_one line = function (regex, digit) -> try_replace line regex digit in
        let replaced = List.fold_left apply_one line replacements in
        match remainder replaced 
        with (hd, tl) -> hd ^ replace_written_nums tl

let print_int_nl i =
    print_int i;
    print_newline ()

let sum acc x = x + acc

(* let part1 = 
    let lines = read_file filename in
    let numbers = List.map get_nums lines in
    let sum = List.fold_left sum 0 numbers in
    print_string "Part 1: ";
    print_int sum;
    print_newline () *)

let part2 = 
    print_string "part 2\n";
    let lines = read_file filename in
    let lines = List.map replace_written_nums lines in
    List.iter (function s -> print_string s; print_newline ()) lines;
    let numbers = List.map get_nums lines in
    let sum = List.fold_left sum 0 numbers in
    print_string "Part 2: ";
    print_int sum;
    print_newline ()

let main () = part2

