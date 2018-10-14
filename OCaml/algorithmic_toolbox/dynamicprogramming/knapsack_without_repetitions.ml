
let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let solve_knapsack_problem weights nr_weights capacity =
    let knapsack = Array.make_matrix capacity nr_weights (-1) in
    let rec solve current_capacity = function
        | [] -> 0
        | (w, ix) :: wss ->
            if current_capacity = 0 then 0
            else if knapsack.(current_capacity - 1).(ix) <> -1 then knapsack.(current_capacity - 1).(ix)
            else
                let optimal_capacity =
                    if current_capacity < w then solve current_capacity wss
                    else max ((solve (current_capacity - w) wss) + w) (solve current_capacity wss) in
                knapsack.(current_capacity - 1).(ix) <- optimal_capacity;
                optimal_capacity
    in solve capacity (List.mapi (fun ix elem -> (elem, ix)) weights)


let () =
    let capacity, nr_weights = (Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)) in
    let weights = read_line() |> convert_to_intlist in
    let result = solve_knapsack_problem weights nr_weights capacity in
    print_int result;
    print_newline()
