let decompose_to_distinct_elems n =
    let rec inner number smallest_summand distinct_summands = match number with
        | 0 -> distinct_summands
        | _ -> let next_summand = if number > 2 * smallest_summand then smallest_summand else number in
               inner (number - next_summand) (smallest_summand + 1) (next_summand :: distinct_summands)
    in inner n 1 []


let () =
    let n = read_int() in
    let result = decompose_to_distinct_elems n in
    print_int (List.length result);
    print_newline();
    List.iter (fun elem -> print_int elem; print_string " ") result;
    print_newline()
