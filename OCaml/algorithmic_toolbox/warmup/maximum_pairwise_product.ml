type largest_elems = {largest : int; second_largest : int}


let convert_to_inlist line = List.map int_of_string (Str.split (Str.regexp " ") line)


let process_next_elem acc x =
    if x > acc.largest then {largest = x; second_largest = acc.largest}
    else if x > acc.second_largest then {acc with second_largest = x}
    else acc


let calc_maximum_pairwise_product list =
    let result = List.fold_left process_next_elem {largest = min_int; second_largest = min_int} list in
    result.largest * result.second_largest


let () =
    let line = read_line () in
    let list = convert_to_inlist line in
    print_int (calc_maximum_pairwise_product list);
    print_newline ()
