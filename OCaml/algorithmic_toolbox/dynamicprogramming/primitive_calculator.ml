let nominators = [2; 3]


let backtrack_calculation backtrack n =
    let rec loop k path =
        if k <= 1 then path
        else
            let m = backtrack.(k) in
            loop m (m + 1 :: path)
    in loop (n - 1) [n]


let find_previous_minimum min_operations k =
    let check_for_shorter_calculation acc nominator =
        let previous_minimum, _ = acc in
        if k mod nominator = 0 then
            let pos = k / nominator - 1 in
            let nr_ops = min_operations.(pos) in
            if nr_ops < previous_minimum then (nr_ops, pos)
            else acc
        else acc in
    let position = k - 2
    in List.fold_left check_for_shorter_calculation ((min_operations.(position), position))  nominators


let run_calculator n =
    let min_operations = Array.make n 0 in
    let backtrack = Array.make n 0 in
    for k=2 to n do
        let previous_minimum, position = find_previous_minimum min_operations k in
        min_operations.(k - 1) <- previous_minimum + 1;
        backtrack.(k - 1) <- position
    done;
    backtrack_calculation backtrack n


let () =
    let n = read_int() in
    let result = run_calculator n in
    print_int (List.length result - 1);
    print_newline();
    List.iter (fun elem -> print_int elem; print_string " ") result;
    print_newline()
