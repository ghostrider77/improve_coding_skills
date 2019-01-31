open Batteries

type edge = { left_end : int; right_end : int; weight : int }
type edge_endpoint = { tip : int; edge_weight : int }

module IntMap = Map.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module IntSet = Set.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module type DirectedGraphSig =
    sig
        type t
        val create_graph : edge list -> int -> t
        val dijkstra : t -> int -> float array
    end

module DirectedGraph : DirectedGraphSig =
    struct
        type t = { adjacency_list : edge_endpoint list IntMap.t; nr_nodes : int }

        let get_neighbours adjacency_list node =
            if IntMap.mem node adjacency_list then IntMap.find node adjacency_list
            else []

        let create_graph edges nr_nodes =
            let add_edge { left_end; right_end; weight } adjacency_list =
                let neighbours = get_neighbours adjacency_list left_end in
                IntMap.add left_end ({ tip = right_end; edge_weight = weight } :: neighbours) adjacency_list in
            let adjacency_list = List.fold_left (fun acc edge -> add_edge edge acc) IntMap.empty edges
            in { adjacency_list; nr_nodes }

        let build_unvisited_nodes start_node nr_nodes =
            let rec loop acc k =
                if k > nr_nodes then acc
                else if k = start_node then loop acc (k + 1)
                else loop (IntSet.add k acc) (k + 1) in
            loop IntSet.empty 1

        let dijkstra { adjacency_list; nr_nodes } start_node =
            let distances = Array.make nr_nodes infinity in
            distances.(start_node - 1) <- 0.0;
            let update_distances node shortest_distance_to_node =
                let neighbours = get_neighbours adjacency_list node in
                let update { tip = v; edge_weight = weight } =
                    let distance_through_node = shortest_distance_to_node +. float weight in
                    if distances.(v - 1) > distance_through_node then distances.(v - 1) <- distance_through_node in
                List.iter update neighbours in
            let rec loop node shortest_distance_to_node unvisited_nodes =
                update_distances node shortest_distance_to_node;
                if not (IntSet.is_empty unvisited_nodes) then
                    let initial_node = IntSet.min_elt unvisited_nodes in
                    let traverse node (current_node, current_min_dist) =
                        if (distances.(node - 1) < current_min_dist) then (node, distances.(node - 1))
                        else (current_node, current_min_dist) in
                    let u, dist_u = IntSet.fold traverse unvisited_nodes (initial_node, distances.(initial_node - 1)) in
                    loop u dist_u (IntSet.remove u unvisited_nodes) in
            let unvisited_nodes = build_unvisited_nodes start_node nr_nodes
            in loop start_node distances.(start_node - 1) unvisited_nodes;
            distances
    end


let read_two_ints() = Scanf.sscanf (read_line ()) "%d %d" (fun x y-> x, y)


let read_weighted_edges() = Scanf.sscanf (read_line ()) "%d %d %d" (fun x y z-> x, y, z)


let read_edges nr_edges =
    let rec loop edges k =
        if k = nr_edges then edges
        else
            let left_end, right_end, weight = read_weighted_edges() in
            loop ({ left_end; right_end; weight } :: edges) (k + 1)
    in loop [] 0


let find_cheapest_path edges nr_nodes start_node end_node =
    let graph = DirectedGraph.create_graph edges nr_nodes in
    let distances_from_start = DirectedGraph.dijkstra graph start_node in
    let distance = distances_from_start.(end_node - 1) in
    if distance = infinity then -1 else int_of_float distance


let () =
    let nr_nodes, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let start_node, end_node = read_two_ints() in
    let result = find_cheapest_path edges nr_nodes start_node end_node in
    print_int result;
    print_newline()
