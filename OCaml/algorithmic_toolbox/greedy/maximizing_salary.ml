let compare_numbers s1 s2 =
    let x = s1 ^ s2
    and y = s2 ^ s1 in
        if x = y then 0
        else if x > y then -1
        else 1


let find_largest_number_from_pieces number_strings = List.sort compare_numbers number_strings


let () =
    let _ = read_int() in
    let number_strings = Str.split (Str.regexp " ") (read_line()) in
    let result = find_largest_number_from_pieces number_strings in
    List.iter (fun elem -> print_string elem; print_string "") result;
    print_newline()
