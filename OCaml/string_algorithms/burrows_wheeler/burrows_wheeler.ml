open Batteries

let calc_burrows_wheeler_transform text =
    let length = String.length text in
    let double_text = text ^ text in
    let compare i j =
        let rec loop k =
            if k = length then 1
            else if String.get double_text (i + k) < String.get double_text (j + k) then -1
            else if String.get double_text (i + k) > String.get double_text (j + k) then 1
            else loop (k + 1)
        in loop 0 in
    let indices = List.init length identity in
    let sorted_indices = List.sort compare indices in
    String.of_list @@ List.map (function ix -> String.get double_text (ix + length - 1)) sorted_indices


let () =
    let text = read_line() in
    let result = calc_burrows_wheeler_transform text in
    print_endline result
