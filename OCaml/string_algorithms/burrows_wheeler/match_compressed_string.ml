module CharDict = Hashtbl.Make(
    struct
        type t = char
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
    end)

module CharMap = Map.Make(
    struct
        type t = char
        let compare = Pervasives.compare
    end)


let calc_first_occurrence_positions string =
    let char_counts = CharDict.create 5 in
    let update_counts c =
        let count = match CharDict.find_opt char_counts c with
            | None -> 0
            | Some value -> value in
        CharDict.replace char_counts c (count + 1) in
    String.iter (function letter -> update_counts letter) string;
    let unique_characters = CharDict.fold (fun key _ acc -> key :: acc) char_counts [] in
    let first_occurences =
        List.fold_left (fun (acc, ix) c -> (CharMap.add c ix acc, ix + CharDict.find char_counts c))
                        (CharMap.empty, 0)
                        (List.sort compare unique_characters)
    in fst first_occurences


let calc_count_matrix string unique_characters =
    let size = String.length string in
    let count_matrix =
        List.fold_left (fun acc c -> CharMap.add c (Array.make (size + 1) 0) acc) CharMap.empty unique_characters in
    String.iteri
        (fun ix letter ->
            List.iter
                (function c ->
                    let counts = CharMap.find c count_matrix
                    in counts.(ix + 1) <- if letter = c then counts.(ix) + 1 else counts.(ix))
                unique_characters)
        string;
    count_matrix


let letter_occurs_between_pointers letter last_column top bottom =
    let rec loop current_ix =
        if current_ix > bottom then false
        else if last_column.[current_ix] = letter then true
        else loop (current_ix + 1)
    in loop top


let pattern_matching last_column first_occurrences count_matrix pattern =
    let rec loop reversed_pattern top bottom = match reversed_pattern with
        | [] -> bottom - top + 1
        | letter :: rest ->
            if not (letter_occurs_between_pointers letter last_column top bottom) then 0
            else
                let letter_occurrence = CharMap.find letter first_occurrences in
                let letter_counter = CharMap.find letter count_matrix in
                loop rest
                     (letter_occurrence + letter_counter.(top))
                     (letter_occurrence + letter_counter.(bottom + 1) - 1)
    in loop (let open Batteries in String.(pattern |> rev |> to_list)) 0 (String.length last_column - 1)


let improved_BW_pattern_matching transformed_string patterns =
    let first_occurrences = calc_first_occurrence_positions transformed_string in
    let unique_letters = CharMap.fold (fun key _ acc -> key :: acc) first_occurrences [] in
    let count_matrix = calc_count_matrix transformed_string unique_letters in
    List.map (pattern_matching transformed_string first_occurrences count_matrix) patterns


let () =
    let transformed_string = read_line() in
    let _ = read_int() in
    let patterns = read_line() |> Str.(split (regexp " ")) in
    let number_of_matches = improved_BW_pattern_matching transformed_string patterns in
    List.iter (function count -> print_int count; print_string " ") number_of_matches;
    print_newline()
