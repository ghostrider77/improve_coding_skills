module IntMap = Map.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module Trie : sig
        type t
        val create : string list -> t
        val to_string_list : t -> string list
    end = struct
        open Batteries

        type node = int
        type edge_endpoint = { tip : node; label : char }
        type t = edge_endpoint list IntMap.t

        let root = 0

        let get_neighbour_with_given_label trie current_node letter =
            Option.map (function { tip } -> tip ) @@
                Option.bind (IntMap.find_opt current_node trie) (List.find_opt (function { label } -> label = letter ))

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

        let to_string_list trie =
            let to_string node neighbour label =
                (string_of_int node) ^ "->" ^ (string_of_int neighbour) ^ ":" ^ (Char.escaped label) in
            List.map
                (function (node, neighbours) ->
                    List.map (function { tip = neighbour; label} -> to_string node neighbour label) neighbours)
                (IntMap.bindings trie) |> List.flatten
    end


let read_patterns n =
    let rec loop acc k =
        if k = n then List.rev acc
        else loop (read_line() :: acc) (k + 1)
    in loop [] 0


let () =
    let number_of_patterns = read_int() in
    let patterns = read_patterns number_of_patterns in
    let trie = Trie.create patterns
    in List.iter print_endline (Trie.to_string_list trie)
