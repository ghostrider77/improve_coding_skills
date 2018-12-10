
type node = { key : int; left_ix : int; right_ix : int }

module type BinaryTreeSig =
    sig
        type t
        val create : node array -> int -> t
        val inorder_traversal : t -> int list
    end

module BinaryTree : BinaryTreeSig =
    struct
        type t = { nodes : node array; root_ix : int }

        let create nodes root_ix = { nodes; root_ix }

        let inorder_traversal { nodes; root_ix } =
            let rec loop keys node_index stack =
                if node_index <> -1 then
                    let node = nodes.(node_index) in
                    loop keys node.left_ix (node :: stack)
                else match stack with
                    | [] -> List.rev keys
                    | { key; right_ix } :: rest -> loop (key :: keys) right_ix rest
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


let is_valid_binary_search_tree tree nr_nodes =
    if nr_nodes <= 1 then true
    else
        let inorder_keys = BinaryTree.inorder_traversal tree in
        is_sorted inorder_keys


let () =
    let nr_nodes = read_int() in
    let nodes = read_node_information nr_nodes in
    let tree = BinaryTree.create nodes 0 in
    let verdict = is_valid_binary_search_tree tree nr_nodes in
    print_endline @@ if verdict then "CORRECT" else "INCORRECT"
