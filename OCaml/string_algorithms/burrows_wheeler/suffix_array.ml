open Batteries

let calc_suffix_array text =
    let length = String.length text in
    let compare i j =
        let limit = min (length - i) (length - j) in
        let rec loop k =
            if k = limit then 1
            else if String.get text (i + k) < String.get text (j + k) then -1
            else if String.get text (i + k) > String.get text (j + k) then 1
            else loop (k + 1)
        in loop 0
    in List.sort compare @@ List.init length identity


let () =
    let text = read_line() in
    let result = calc_suffix_array text in
    List.iter (function elem -> print_int elem; print_string " ") result;
    print_newline()
