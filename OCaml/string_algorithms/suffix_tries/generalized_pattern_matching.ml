let string_terminator = '$'


module IntMap = Map.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module Trie : sig
        type t
        val create : string list -> t
        val pattern_starts : char list -> t -> bool
    end = struct
        open Batteries

        type node = int
        type edge_endpoint = { tip : node; label : char }
        type t = edge_endpoint list IntMap.t

        let root = 0

        let get_neighbour_with_given_label trie current_node letter =
            Option.map (function { tip } -> tip ) @@
                Option.bind (IntMap.find_opt current_node trie) (List.find_opt (function { label } -> label = letter ))

        let has_leaf_child node trie = match IntMap.find_opt node trie with
            | None -> false
            | Some children -> List.exists (function { label } -> label = string_terminator) children

        let add_word trie w node_counter =
            let rec loop acc current_node = function
                | [] -> acc
                | letter :: lss -> match (get_neighbour_with_given_label acc current_node letter) with
                    | None ->
                        let next_node = !node_counter in
                        node_counter := !node_counter + 1;
                        let children = match IntMap.find_opt current_node acc with
                            | None -> []
                            | Some values -> values in
                        let updated_trie = IntMap.add current_node ({ tip = next_node; label = letter } :: children) acc
                        in loop updated_trie next_node lss
                    | Some node -> loop acc node lss
            in loop trie root (String.to_list w)

        let create words =
            let node_counter = ref 1 in
            let rec loop trie = function
                | [] -> trie
                | w :: wss ->
                    let updated_trie = add_word trie w node_counter
                    in loop updated_trie wss
            in loop IntMap.empty words

        let pattern_starts text trie =
            let rec loop current_node = function
                | [] -> false
                | letter :: lss -> match (get_neighbour_with_given_label trie current_node letter) with
                    | None -> false
                    | Some next_node -> if has_leaf_child next_node trie then true else loop next_node lss
            in loop root text
    end


let append_terminator string = string ^ Char.escaped string_terminator


let multiple_pattern_matching text patterns =
    let trie = Trie.create patterns in
    let rec loop indices suffix ix = match suffix with
        | [] -> List.rev indices
        | _ :: rest ->
            let updated_indices = if Trie.pattern_starts suffix trie then ix :: indices else indices in
            loop updated_indices rest (ix + 1) in
    let char_list = let open Batteries in String.to_list text in
    loop [] char_list 0


let read_patterns n =
    let rec loop acc k =
        if k = n then List.rev acc
        else loop (read_line() :: acc) (k + 1)
    in loop [] 0


let () =
    let text = read_line() in
    let number_of_patterns = read_int() in
    let patterns = number_of_patterns |> read_patterns |> List.map append_terminator in
    let result = multiple_pattern_matching text patterns in
    List.iter (function ix -> print_int ix; print_string " ") result;
    print_newline()
