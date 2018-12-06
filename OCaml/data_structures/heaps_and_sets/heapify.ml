let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let get_index_of_parent_children_minimum arr parent_index size =
    let left_child_ix = 2 * parent_index + 1 in
    let right_child_ix = left_child_ix + 1 in
    let min_index =
        if left_child_ix < size && arr.(left_child_ix) < arr.(parent_index) then left_child_ix
        else parent_index in
    if right_child_ix < size && arr.(right_child_ix) < arr.(min_index) then right_child_ix else min_index


let swap_elems arr ix jy =
    let elem = arr.(ix) in
    arr.(ix) <- arr.(jy);
    arr.(jy) <- elem


let sift_down arr parent_ix n =
    let rec loop current_parent_ix current_min_ix swaps =
        if current_min_ix = current_parent_ix then swaps
        else
            let () = swap_elems arr current_min_ix current_parent_ix in
            let next_parent_ix = current_min_ix in
            let next_min_ix = get_index_of_parent_children_minimum arr next_parent_ix n in
            loop next_parent_ix next_min_ix ((current_parent_ix, current_min_ix) :: swaps) in
    let min_index = get_index_of_parent_children_minimum arr parent_ix n
    in loop parent_ix min_index []


let heapify arr n =
    let rec loop swaps parent_index =
        if parent_index < 0 then List.rev swaps
        else
            let current_swaps = sift_down arr parent_index n in
            loop (current_swaps @ swaps) (parent_index - 1)
    in loop [] (n / 2  - 1)


let () =
    let n = read_int() in
    let arr = Array.of_list @@ convert_to_intlist (read_line()) in
    let swaps = heapify arr n in
    print_int (List.length swaps);
    print_newline();
    List.iter (function (i, j) -> print_endline (string_of_int i ^ " " ^ string_of_int j)) swaps
