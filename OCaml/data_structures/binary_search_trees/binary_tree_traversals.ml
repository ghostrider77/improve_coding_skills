
type node = { key : int; left_ix : int; right_ix : int }

module type BinaryTreeSig =
    sig
        type t
        val create : node array -> int -> t
        val inorder_traversal : t -> int list
        val preorder_traversal : t -> int list
        val postorder_traversal : t -> int list
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

        let preorder_traversal { nodes; root_ix } =
            let rec loop keys node_index stack =
                if node_index <> -1 then
                    let { key; left_ix; right_ix } = nodes.(node_index) in
                    loop (key :: keys) left_ix (right_ix :: stack)
                else match stack with
                    | [] -> List.rev keys
                    | ix :: rest -> loop keys ix rest
            in loop [] root_ix []

        let postorder_traversal { nodes; root_ix } =
            let rec find_node_order stack1 stack2 = match stack1 with
                | [] -> stack2
                | ix :: rest ->
                    if ix = -1 then find_node_order rest stack2
                    else
                        let { left_ix; right_ix } = nodes.(ix) in
                        find_node_order (right_ix :: left_ix :: rest) (ix :: stack2) in
            let ordered_indices = find_node_order [root_ix] [] in
            List.map (function ix -> nodes.(ix).key) ordered_indices
    end


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let read_node_information nr_nodes =
    let rec loop nodes k =
        if k = nr_nodes then Array.of_list @@ List.rev nodes
        else
            let key, left, right = Scanf.sscanf (read_line()) "%d %d %d" (fun x y z -> x, y, z) in
            loop ({ key; left_ix = left; right_ix = right} :: nodes) (k + 1)
    in loop [] 0


let () =
    let nr_nodes = read_int() in
    let nodes = read_node_information nr_nodes in
    let tree = BinaryTree.create nodes 0 in
    let inorder = BinaryTree.inorder_traversal tree in
    let preorder = BinaryTree.preorder_traversal tree in
    let postorder = BinaryTree.postorder_traversal tree in
    List.iter (function elem -> print_int elem; print_string " ") inorder;
    print_newline();
    List.iter (function elem -> print_int elem; print_string " ") preorder;
    print_newline();
    List.iter (function elem -> print_int elem; print_string " ") postorder;
    print_newline()
