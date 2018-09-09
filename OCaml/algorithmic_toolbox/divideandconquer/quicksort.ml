let () = Random.init 2112

let convert_to_intlist line = List.map int_of_string (Str.split (Str.regexp " ") line)


let three_way_partitioning arr pivot start_index end_index =
    let swap_elems ix elem jy =
        (arr.(ix) <- arr.(jy);
         arr.(jy) <- elem) in
    let rec loop ix middle_start middle_end =
        if ix > middle_end then (middle_start, middle_end)
        else
            let elem = arr.(ix) in
            if elem < pivot then
                (if ix <> middle_start then swap_elems ix elem middle_start;
                loop (ix + 1) (middle_start + 1) middle_end)
            else if elem > pivot then
                (swap_elems ix elem middle_end;
                loop ix middle_start (middle_end - 1))
            else loop (ix + 1) middle_start middle_end
    in loop start_index start_index end_index


let quicksort arr n =
    let rec loop stack =
        if (stack <> []) then
            let left_end, right_end = List.hd stack in
            if left_end >= right_end then loop (List.tl stack)
            else
                let random_ix = left_end + Random.int (right_end - left_end + 1) in
                let pivot = arr.(random_ix) in
                let middle_start, middle_end = three_way_partitioning arr pivot left_end right_end in
                loop ((middle_end + 1, right_end) :: ((left_end, middle_start - 1) :: (List.tl stack)))
    in loop [(0, n - 1)]


let () =
    let n = read_int() in
    let arr = convert_to_intlist (read_line()) |> Array.of_list in
    quicksort arr n;
    Array.iter (fun elem -> print_int elem; print_string " ") arr;
    print_newline()
