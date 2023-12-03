open Common

type draw = Draw of (int * string) list
[@@deriving show]

type game = Game of int * (draw list)
[@@deriving show]

module StringMap = Map.Make (String)

let filename = "inputs/day2.txt"

let parse_draw string =
    let num_col_lst = String.split_on_char ',' string |> List.map String.trim in
    let f s = 
        let parts = String.split_on_char ' ' s  in
        let num = List.nth parts 0 |> int_of_string in
        let color = List.nth parts 1 in
        (num, color)
    in
    Draw (List.map f num_col_lst)

let line_to_game line = 
    let parts = String.split_on_char ':' line in
    let game_parts = String.split_on_char ' ' (List.nth parts 0) in
    let game_id = List.nth game_parts 1 |> int_of_string in
    let draws = String.split_on_char ';' (List.nth parts 1) |> (List.map parse_draw) in
    Game (game_id, draws)

let draw_color = function | (_, color) -> color
let draw_num = function | (num, _) -> num
let val_or default = function | Some (v) -> v | _ -> default
let game_draws = function | Game (_, draws) -> draws
let game_id = function | Game (id, _) -> id

let draw_to_counts = function | Draw (draw_list) ->
    let f acc elem = 
        let key = draw_color elem in
        let current = StringMap.find_opt key acc |> (val_or 0) in
        StringMap.add key (current + draw_num elem) acc in
    List.fold_left f StringMap.empty draw_list

let max_opt a b = match a with
    | Some(va) -> (match b with
        | Some(vb) -> max va vb
        | None -> va)
    | None -> match b with
        | Some(vb) -> vb
        | None -> 0

let counts_max a b = 
    let red = max_opt (StringMap.find_opt "red" a) (StringMap.find_opt "red" b) in
    let green = max_opt (StringMap.find_opt "green" a) (StringMap.find_opt "green" b) in
    let blue = max_opt (StringMap.find_opt "blue" a) (StringMap.find_opt "blue" b) in
    StringMap.empty |> StringMap.add "red" red |> StringMap.add "green" green |> StringMap.add "blue" blue

let get_max_counts game =
    let counts_0 = List.nth (game_draws game) 0 |> draw_to_counts in
    let counts = game_draws game |> List.map draw_to_counts in
    List.fold_left counts_max counts_0 counts

let counts_to_power counts = 
    let red = StringMap.find_opt "red" counts |> (val_or 0) in
    let blue = StringMap.find_opt "blue" counts |> (val_or 0) in
    let green = StringMap.find_opt "green" counts |> (val_or 0) in
    red * blue * green

let check_counts counts =
    let red = StringMap.find_opt "red" counts |> (val_or 0) in
    let blue = StringMap.find_opt "blue" counts |> (val_or 0) in
    let green = StringMap.find_opt "green" counts |> (val_or 0) in
    let result = (red <= 12) && (green <= 13) && (blue <= 14) in
    result

let is_possible game = 
    let counts = List.map draw_to_counts (game_draws game) in
    let possible_counts = List.filter check_counts counts in
    (List.length possible_counts) == (List.length counts)

let part1 () =
    let games = read_file filename |> (List.map line_to_game) in
    let possible_games = List.filter is_possible games in
    let sum_ids = List.fold_left (fun sum game -> sum + game_id game) 0 possible_games in
    print_int sum_ids ; print_newline () ;
    let powers_sum = List.map get_max_counts games 
        |> (List.map counts_to_power) 
        |> List.fold_left (fun acc curr -> acc + curr) 0 in
    print_string "Power sum = " ; print_int powers_sum ; print_newline () ;
    ()

let main () = part1 ()

