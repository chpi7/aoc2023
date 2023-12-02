let rec read_lines ic acc =
    try
        let line = input_line ic in 
        (line :: read_lines ic acc)
    with End_of_file -> acc

let read_file fname =
    let ic = open_in fname in
    let lines = read_lines ic [] in
    close_in ic;
    lines

