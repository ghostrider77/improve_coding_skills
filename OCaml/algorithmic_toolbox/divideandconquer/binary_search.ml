let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let find_query_element input_vector length query =
    let rec binary_search left right =
        if (left > right) then -1
        else let middle_ix = (left + right) / 2 in
             let middle_elem = input_vector.(middle_ix) in
                 if (middle_elem = query) then middle_ix
                 else if (middle_elem < query) then binary_search (middle_ix + 1) right
                 else binary_search left (middle_ix - 1)
    in binary_search 0 (length - 1)


let find_elems_in_list lst n queries =
    let input_vector = Array.of_list lst in
    List.map (find_query_element input_vector n) queries


let () =
    let input_list = convert_to_intlist (read_line()) in
    let query_list = convert_to_intlist (read_line()) in
    let result = find_elems_in_list (List.tl input_list) (List.hd input_list) (List.tl query_list) in
    List.iter (fun elem -> print_int elem; print_string " ") result;
    print_newline()
