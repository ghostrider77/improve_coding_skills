type item = {value : int; weight : int}


let read_items nr_items =
    let pair_to_item value weight = {value=value; weight=weight} in
    let rec read_item n acc = match n with
        | 0 -> acc
        | _ -> read_item (n - 1) ((Scanf.sscanf (read_line ()) "%d %d" pair_to_item) :: acc)
    in read_item nr_items []


let comparison item1 item2 =
    let value_divided_by_weight it = (float it.value /. float it.weight) in
    compare (value_divided_by_weight item1) (value_divided_by_weight item2)


let reverse_comparison item1 item2 = -(comparison item1 item2)


let solve_fractional_knapsack items capacity =
    let sorted_items = List.sort reverse_comparison items in
    let rec inner current_capacity xs total_value = match current_capacity, xs with
        | 0, _ | _, [] -> total_value
        | _, ({value=v; weight=w} :: xss) ->
            let used_amount = min current_capacity w in
            let increased_value = total_value +. float ((used_amount * v)) /. (float w)
            in inner (current_capacity - used_amount) xss increased_value
    in inner capacity sorted_items 0.0


let () =
    let nr_items, capacity = (Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)) in
    let items = read_items nr_items in
    print_float (solve_fractional_knapsack items capacity);
    print_newline ()
