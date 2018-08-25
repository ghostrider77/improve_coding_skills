let convert_to_inlist line = List.map int_of_string (Str.split (Str.regexp " ") line)


let calc_maximal_revenue profit_per_click average_click_per_day =
    let zipped = List.combine (List.sort compare profit_per_click) (List.sort compare average_click_per_day)
    in List.fold_left (fun acc (p, a) -> acc + p * a) 0 zipped


let () =
    let _ = read_int() in
    let profit_per_click = convert_to_inlist (read_line()) in
    let average_click_per_day = convert_to_inlist (read_line()) in
    print_int (calc_maximal_revenue profit_per_click average_click_per_day);
    print_newline()
