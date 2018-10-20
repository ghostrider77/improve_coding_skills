let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let calc_maximal_revenue profit_per_click average_click_per_day =
    List.fold_left2
        (fun acc p c -> acc + p * c) 0 (List.sort compare profit_per_click) (List.sort compare average_click_per_day)


let () =
    let _ = read_int() in
    let profit_per_click = convert_to_intlist (read_line()) in
    let average_click_per_day = convert_to_intlist (read_line()) in
    print_int (calc_maximal_revenue profit_per_click average_click_per_day);
    print_newline()
