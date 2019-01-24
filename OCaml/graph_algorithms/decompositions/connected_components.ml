type edge = { left_end : int; right_end : int }

module IntDict = Hashtbl.Make(
    struct
        type t = int
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
    end)

module type GraphSig =
    sig
        type t
        type component = int list
        val create_graph : edge list -> int -> t
        val get_neighbours : t -> int -> int list
        val connected_components : t -> component list
    end

module Graph : GraphSig =
    struct
        type t = { adjacency_list : int list IntDict.t; nr_nodes : int }
        type component = int list

        let reverse { left_end = a; right_end = b } = { left_end = b; right_end = a }

        let range a b =
            let rec loop acc left =
                if left >= b then List.rev acc
                else loop (left :: acc) (left + 1)
            in loop [] a

        let get_neighbours { adjacency_list } node =
            match IntDict.find_opt adjacency_list node with
                | None -> []
                | Some neighbours -> neighbours

        let create_graph edges nr_nodes =
            let adjacency_list = IntDict.create 100 in
            let add_edge { left_end; right_end } =
                let neighbours = get_neighbours { adjacency_list; nr_nodes } left_end in
                IntDict.replace adjacency_list left_end (right_end :: neighbours) in
            List.iter add_edge edges;
            List.iter (function e -> add_edge @@ reverse e) edges;
            { adjacency_list; nr_nodes }

        let connected_components ({ nr_nodes } as graph) =
            let visit_started = Array.make nr_nodes 0 in
            let visit_ended = Array.make nr_nodes 0 in
            let previsit_id = ref 1 in
            let postvisit_id = ref 1 in
            let is_node_visited node = visit_started.(node - 1) > 0 in
            let find_unvisited_neighbour node =
                List.find_opt (function elem -> not (is_node_visited elem)) (get_neighbours graph node) in
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
                        find_components remaining_nodes (current_component :: components)
            in find_components (range 1 (nr_nodes + 1)) []
    end


let read_two_ints() = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)


let read_edges nr_edges =
    let rec loop edges k =
        if k = nr_edges then edges
        else
            let left_end, right_end = read_two_ints() in
            loop ({ left_end; right_end } :: edges) (k + 1)
    in loop [] 0


let () =
    let nr_nodes, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let graph = Graph.create_graph edges nr_nodes in
    let components = Graph.connected_components graph in
    print_int (List.length components);
    print_newline()
