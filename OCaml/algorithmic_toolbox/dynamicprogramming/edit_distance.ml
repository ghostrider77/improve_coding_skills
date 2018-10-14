open Batteries

let calc_Levenshtein_distance string1 string2 =
    let n = String.length string1 in
    let m = String.length string2 in
    let edit_distance = Array.make_matrix (n + 1) (m + 1) 0 in
    let matching_score c1 c2 = if c1 = c2 then 0 else 1 in
    for ix = 1 to n do
        edit_distance.(ix).(0) <- ix
    done;
    for jy = 1 to m do
        edit_distance.(0).(jy) <- jy
    done;

    for ix = 1 to n do
        for jy = 1 to m do
            let deletion = edit_distance.(ix - 1).(jy) + 1 in
            let insertion = edit_distance.(ix).(jy - 1) + 1 in
            let matching = edit_distance.(ix  -1).(jy - 1) +
                matching_score (String.get string1 (ix - 1)) (String.get string2 (jy - 1)) in
            edit_distance.(ix).(jy) <- List.min [deletion; insertion; matching]
        done;
    done;
    edit_distance.(n).(m)


let () =
    let string1 = read_line() in
    let string2 = read_line() in
    let result = calc_Levenshtein_distance string1 string2 in
    print_int result;
    print_newline()
