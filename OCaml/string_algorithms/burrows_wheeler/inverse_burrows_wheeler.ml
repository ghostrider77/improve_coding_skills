module CharDict = Hashtbl.Make(
    struct
        type t = char
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
    end)

module PairDict = Hashtbl.Make(
    struct
        type t = char * int
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
    end)


let string_sort string =
    let explode s = Array.init (String.length s) (String.get s) in
    let collapse arr = String.init (Array.length arr) (Array.get arr) in
    let arr = explode string in
    Array.fast_sort compare arr;
    collapse arr


let create_indexed_column string =
    let size = String.length string in
    let counts = CharDict.create 5 in
    let update_counts letter =
        let count = match CharDict.find_opt counts letter with
            | None -> 0
            | Some value -> value in
        CharDict.replace counts letter (count + 1);
        count
    in Array.init size (function ix -> let character = String.get string ix in (character, update_counts character))


let create_first_column string =
    let sorted = string_sort string in
    let table = PairDict.create @@ String.length string in
    let insert charnumber k = PairDict.add table charnumber k in
    sorted |> create_indexed_column |> Array.iteri (fun ix numberedchar -> insert numberedchar ix);
    table


let inverse_burrows_wheeler_transform transformed_string =
    let last_column = create_indexed_column transformed_string in
    let first_column = create_first_column transformed_string in
    let size = String.length transformed_string in
    let rec loop acc position current_ix =
        if current_ix = size then acc
        else
            let (character, _) as item = last_column.(position)
            in loop (character :: acc) (PairDict.find first_column item) (current_ix + 1) in
    let reconstructed = loop ['$'] 0 0 in
    reconstructed |> List.tl |> (let open Batteries in String.of_list)


let () =
    let transformed_string = read_line() in
    let original_text = inverse_burrows_wheeler_transform transformed_string in
    print_endline original_text
