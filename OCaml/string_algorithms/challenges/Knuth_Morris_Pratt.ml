let calc_prefix_function text =
    let size = String.length text in
    let prefix_array = Array.make size 0 in
    let border = ref 0 in
    for ix = 1 to (size - 1) do
        let letter = text.[ix] in
        while !border > 0 && letter <> text.[!border] do
            border := prefix_array.(!border - 1)
        done;
        if letter = text.[!border] then border := !border + 1
        else border := 0;
        prefix_array.(ix) <- !border
    done;
    prefix_array


let find_pattern_in_genome genome pattern =
    let pattern_length = String.length pattern in
    let prefix_array = calc_prefix_function (pattern ^ "$" ^ genome) in
    let pattern_starts starting_indices ix item =
        if ix > pattern_length && item = pattern_length then ix - 2 * pattern_length :: starting_indices
        else starting_indices in
    let open Batteries in
    prefix_array |> Array.fold_lefti pattern_starts [] |> List.rev


let () =
    let pattern = read_line() in
    let genome = read_line() in
    let indices = find_pattern_in_genome genome pattern in
    List.iter (function ix -> print_int ix; print_string " ") indices;
    print_newline()
