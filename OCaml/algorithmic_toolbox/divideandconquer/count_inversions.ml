open Batteries

let convert_to_intlist line = List.map int_of_string (Str.split (Str.regexp " ") line)


let merge_sorted_arrays first second length1 inversions =
    let rec merge xs xs_length ys acc total_inversions = match (xs, ys) with
        | [], [] -> List.rev acc, total_inversions
        | [], y :: yss -> merge [] xs_length yss (y :: acc) total_inversions
        | x :: xss, [] -> merge xss (xs_length - 1) [] (x :: acc) total_inversions
        | x :: xss, y :: yss ->
            if (x <= y) then merge xss (xs_length - 1) ys (x :: acc) total_inversions
            else merge xs xs_length yss (y :: acc) (total_inversions + xs_length)
    in merge first length1 second [] inversions


let rec count_inversions lst length =
    if length <= 1 then lst, 0
    else
        let middle = length / 2 in
        let first, second = List.split_at middle lst in
        let length1, length2 = middle, length - middle in
        let sorted_first, inversions_first = count_inversions first length1 in
        let sorted_second, inversions_second = count_inversions second length2 in
        merge_sorted_arrays sorted_first sorted_second length1 (inversions_first + inversions_second)


let () =
    let n = read_int() in
    let lst = convert_to_intlist (read_line()) in
    let _, result = count_inversions lst n in
    print_int result;
    print_newline()
