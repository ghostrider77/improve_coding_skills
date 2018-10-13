
let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let read_intervals nr_segments =
    let rec read_segment n acc = match n with
        | 0 -> acc
        | _ -> read_segment (n - 1) ((Scanf.sscanf (read_line ()) "%d %d" (fun x y -> (x, y))) :: acc)
    in List.split (read_segment nr_segments [])


let get_number_of_suitable_endpoints endpoints length point =
    let rec binary_search a b =
        if a = b then a
        else
            let mid = (a + b) / 2 in
            if endpoints.(mid) <= point then binary_search (mid + 1) b
            else binary_search a mid in
    if endpoints.(length-1) <= point then length
    else binary_search 0 (length - 1)


let calc_intersection_size sorted_left sorted_negated_right nr_segments point =
    let nr_good_left_ends = get_number_of_suitable_endpoints sorted_left nr_segments point in
    let nr_good_right_ends = get_number_of_suitable_endpoints sorted_negated_right nr_segments (-point)
    in nr_good_left_ends + nr_good_right_ends - nr_segments


let number_of_segments_containing_points left_endpoints right_endpoints nr_segments points =
    let sorted_left = left_endpoints |> List.sort compare |> Array.of_list in
    let sorted_negated_right = right_endpoints |> List.map (~-) |> List.sort compare |> Array.of_list in
    List.map (calc_intersection_size sorted_left sorted_negated_right nr_segments) points


let () =
    let nr_segments, _ = (Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)) in
    let left_endpoints, right_endpoints = read_intervals nr_segments in
    let points = convert_to_intlist (read_line()) in
    let result = number_of_segments_containing_points left_endpoints right_endpoints nr_segments points in
    List.iter (fun elem -> print_int elem; print_string " ") result;
    print_newline()
