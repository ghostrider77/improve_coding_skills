
type node = { key : int; left_ix : int; right_ix : int }

module type BinaryTreeSig =
    sig
        type t
        val nodes : t -> node array
        val create : node array -> int -> t
        val inorder_traversal : t -> int list
    end

module BinaryTree : BinaryTreeSig =
    struct
        type t = { nodes : node array; root_ix : int }

        let create nodes root_ix = { nodes; root_ix }

        let nodes { nodes } = nodes

        let inorder_traversal { nodes; root_ix } =
            let rec loop node_indices node_index stack =
                if node_index <> -1 then
                    let { left_ix } = nodes.(node_index) in
                    loop node_indices left_ix (node_index :: stack)
                else match stack with
                    | [] -> List.rev node_indices
                    | ix :: rest ->
                        let { right_ix } = nodes.(ix) in
                        loop (ix :: node_indices) right_ix rest
            in loop [] root_ix []
    end


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let read_node_information nr_nodes =
    let rec loop nodes k =
        if k = nr_nodes then Array.of_list @@ List.rev nodes
        else
            let key, left, right = Scanf.sscanf (read_line()) "%d %d %d" (fun x y z -> x, y, z) in
            loop ({ key; left_ix = left; right_ix = right} :: nodes) (k + 1)
    in loop [] 0


let rec is_sorted = function
    | [] | [_] -> true
    | a :: (b :: rest as lst) -> if a <= b then is_sorted lst else false


let are_duplicates_in_right_subtree keys inorder_indices_of_nodes nodes =
    let rec loop ix = function
        | [] -> true
        | index_of_node :: ixss ->
            let { key; left_ix } = nodes.(index_of_node) in
            if left_ix = -1 then loop (ix + 1) ixss
            else
                let key_to_the_left = keys.(ix - 1) in
                if key_to_the_left = key then false
                else loop (ix + 1) ixss
    in loop 0 inorder_indices_of_nodes


let is_valid_binary_search_tree tree nr_nodes =
    if nr_nodes <= 1 then true
    else
        let inorder_indices_of_nodes = BinaryTree.inorder_traversal tree in
        let nodes = BinaryTree.nodes tree in
        let keys = List.map (function ix -> nodes.(ix).key ) inorder_indices_of_nodes in
        is_sorted keys && are_duplicates_in_right_subtree (Array.of_list keys) inorder_indices_of_nodes nodes


let () =
    let nr_nodes = read_int() in
    let nodes = read_node_information nr_nodes in
    let tree = BinaryTree.create nodes 0 in
    let verdict = is_valid_binary_search_tree tree nr_nodes in
    print_endline @@ if verdict then "CORRECT" else "INCORRECT"
