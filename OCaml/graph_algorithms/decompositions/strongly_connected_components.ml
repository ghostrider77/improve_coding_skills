open Batteries

type edge = { left_end : int; right_end : int }
type component = int list
type dfs_output = { components : component list; postvisit_numbers : int list }

module IntDict = Hashtbl.Make(
    struct
        type t = int
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
    end)

module type DirectedGraphSig =
    sig
        type t
        val create_graph : edge list -> int -> int list option -> t
        val range: int -> int -> int list
        val depth_first_search : t -> dfs_output
    end

module DirectedGraph : DirectedGraphSig =
    struct
        type t = { adjacency_list : int list IntDict.t; nr_nodes : int; ordered_nodes : int list }

        let range a b =
            let rec loop acc left =
                if left >= b then List.rev acc
                else loop (left :: acc) (left + 1)
            in loop [] a

        let get_neighbours adjacency_list node =
            match IntDict.find_option adjacency_list node with
                | None -> []
                | Some neighbours -> neighbours

        let create_graph edges nr_nodes ordered_nodes =
            let adjacency_list = IntDict.create 100 in
            let add_edge { left_end; right_end } =
                let neighbours = get_neighbours adjacency_list left_end in
                IntDict.replace adjacency_list left_end (right_end :: neighbours) in
            List.iter add_edge edges;
            match ordered_nodes with
                | None -> { adjacency_list; nr_nodes; ordered_nodes = (range 1 (nr_nodes + 1)) }
                | Some node_ordering -> { adjacency_list; nr_nodes; ordered_nodes = node_ordering }

        let depth_first_search { adjacency_list; nr_nodes; ordered_nodes } =
            let visit_started = Array.make nr_nodes 0 in
            let visit_ended = Array.make nr_nodes 0 in
            let previsit_id = ref 1 in
            let postvisit_id = ref 1 in
            let is_node_visited node = visit_started.(node - 1) > 0 in
            let find_unvisited_neighbour node =
                List.find_opt (function elem -> not (is_node_visited elem)) (get_neighbours adjacency_list node) in
            let explore starting_node =
                let rec traverse_component previsit_stack component = match previsit_stack with
                    | [] -> component
                    | node :: rest_of_stack -> match find_unvisited_neighbour node with
                        | Some neighbour ->
                            visit_started.(neighbour - 1) <- !previsit_id;
                            previsit_id := !previsit_id + 1;
                            traverse_component (neighbour :: previsit_stack) (neighbour :: component)
                        | None ->
                            visit_ended.(node - 1) <- !postvisit_id;
                            postvisit_id := !postvisit_id + 1;
                            traverse_component rest_of_stack component in
                visit_started.(starting_node - 1) <- !previsit_id;
                previsit_id := !previsit_id + 1;
                traverse_component [starting_node] [starting_node] in
            let rec find_components nodes components = match nodes with
                | [] -> components
                | node :: remaining_nodes ->
                    if is_node_visited node then find_components remaining_nodes components
                    else
                        let current_component = explore node in
                        find_components remaining_nodes (current_component :: components) in
            let components = find_components ordered_nodes [] in
            { components; postvisit_numbers = visit_ended |> Array.to_list }
    end


let read_two_ints() = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)


let read_edges nr_edges =
    let rec loop edges k =
        if k = nr_edges then edges
        else
            let left_end, right_end = read_two_ints() in
            loop ({ left_end; right_end } :: edges) (k + 1)
    in loop [] 0


let create_graph_with_edges_reversed edges nr_nodes postvisit_ordering =
    let reversed_edges = List.map (function { left_end = a; right_end = b } -> { left_end = b; right_end = a }) edges in
    let cmp (post_number1, _) (post_number2, _) = Pervasives.compare post_number2 post_number1 in
    let zipped =  List.combine postvisit_ordering (DirectedGraph.range 1 (nr_nodes + 1)) in
    let node_order = snd @@ List.split @@ List.sort cmp @@ zipped in
    DirectedGraph.create_graph reversed_edges nr_nodes (Some node_order)


let calc_strongly_connected_components edges nr_nodes =
    let graph = DirectedGraph.create_graph edges nr_nodes None in
    let { postvisit_numbers } = DirectedGraph.depth_first_search graph in
    let reversed_graph = create_graph_with_edges_reversed edges nr_nodes postvisit_numbers in
    let { components } = DirectedGraph.depth_first_search reversed_graph
    in components


let () =
    let nr_nodes, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let scc = calc_strongly_connected_components edges nr_nodes in
    print_int (List.length scc);
    print_newline()
