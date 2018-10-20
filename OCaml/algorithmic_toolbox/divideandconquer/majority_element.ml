module IntMap =
    Map.Make(
        struct
            type t = int
            let compare = Pervasives.compare
        end
        )

let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let count_elements lst =
    let update map elem =
        let value = if (IntMap.mem elem map) then (IntMap.find elem map) else 0 in
        IntMap.add elem (value + 1) map
    in List.fold_left update IntMap.empty lst


let has_majority_elem lst n =
    let counter = count_elements lst
    in IntMap.exists (fun _ count -> count > n / 2) counter


let () =
    let n = read_int() in
    let lst = convert_to_intlist (read_line()) in
    print_int (if (has_majority_elem lst n) then 1 else 0);
    print_newline()
