type segment = {left : int; right : int}

let read_segments nr_segments =
    let pair_to_segment left right = {left; right} in
    let rec read_segment n acc = match n with
        | 0 -> acc
        | _ -> read_segment (n - 1) ((Scanf.sscanf (read_line ()) "%d %d" pair_to_segment) :: acc)
    in read_segment nr_segments []


let calc_minimum_number_of_points_covering_segments segments =
    let sorted_segments = List.sort (fun s1 s2 -> s1.right - s2.right) segments in
    let rec inner remaining_segments points = match remaining_segments with
        | [] -> points
        | {right} :: rest -> inner (List.filter (function {left} -> left > right) rest) (right :: points)
    in inner sorted_segments []


let () =
    let nr_segments = read_int() in
    let segments = read_segments nr_segments in
    let covering = calc_minimum_number_of_points_covering_segments segments in
    print_int (List.length covering);
    print_newline();
    List.iter (function elem -> print_int elem; print_string " ") covering;
    print_newline()
