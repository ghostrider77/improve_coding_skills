open Batteries

let allowed_operations = [('+', (+)); ('-', (-)); ('*', ( * ))]


let read_input_data line =
    let digits, operations = List.partition Char.is_digit (String.to_list line) in
    let int_of_digit c = c |> Char.escaped |> int_of_string
    in (List.map int_of_digit digits, Array.of_list operations)


let calc_min_max ix jy operations minimum_of_subexpressions maximum_of_subexpressions =
    let calc_subexpression_values_in_range (subexpression_min, subexpression_max) k =
        let op = List.assoc operations.(k) allowed_operations in
        let subexpressions_split_at_k =
            [op maximum_of_subexpressions.(ix).(k) maximum_of_subexpressions.(k + 1).(jy);
             op maximum_of_subexpressions.(ix).(k) minimum_of_subexpressions.(k + 1).(jy);
             op minimum_of_subexpressions.(ix).(k) maximum_of_subexpressions.(k + 1).(jy);
             op minimum_of_subexpressions.(ix).(k) minimum_of_subexpressions.(k + 1).(jy)]
        in (min subexpression_min (List.min subexpressions_split_at_k),
            max subexpression_max (List.max subexpressions_split_at_k)) in
    let min_max = ref (max_int, min_int) in
    for k = ix to (jy - 1) do
        min_max := calc_subexpression_values_in_range !min_max k
    done;
    !min_max


let maximize_expression digits operations =
    let n = List.length digits in
    let minimum_of_subexpressions = Array.make_matrix n n 0 in
    let maximum_of_subexpressions = Array.make_matrix n n 0 in
    List.iteri
        (fun ix digit ->
            minimum_of_subexpressions.(ix).(ix) <- digit;
            maximum_of_subexpressions.(ix).(ix) <- digit)
        digits;
    for s = 1 to (n - 1) do
        for ix = 0 to (n - s - 1) do
            let jy = ix + s in
            let sub_min, sub_max = calc_min_max ix jy operations minimum_of_subexpressions maximum_of_subexpressions in
            minimum_of_subexpressions.(ix).(jy) <- sub_min;
            maximum_of_subexpressions.(ix).(jy) <- sub_max
        done;
    done;
    maximum_of_subexpressions.(0).(n - 1)


let () =
    let digits, operations = read_line() |> read_input_data in
    let result = maximize_expression digits operations in
    print_int result;
    print_newline()
