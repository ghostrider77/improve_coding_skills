open Batteries

type edge = { left_end : int; right_end : int; weight : int }
type edge_endpoint = { tip : int; edge_weight : int }

module IntMap = Map.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module type DirectedGraphSig =
    sig
        type t
        val create_graph : edge list -> int -> t
        val has_negative_cycle : t -> bool
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

        let update_distances adjacency_list distances =
            let update node neighbours acc =
                let dist_node = distances.(node - 1) in
                let update_neighbour update_happened { tip = neighbour; edge_weight = weight } =
                    let distance_through_node = dist_node + weight in
                    if distances.(neighbour - 1) > distance_through_node then
                        (distances.(neighbour - 1) <- distance_through_node;
                         true)
                    else update_happened in
                let any_neighbour_updated = List.fold_left update_neighbour false neighbours in
                acc || any_neighbour_updated
            in IntMap.fold update adjacency_list false

        let has_negative_cycle { adjacency_list; nr_nodes } =
            let distances = Array.make nr_nodes 0 in
            let rec loop complete_pass_on_edges =
                if complete_pass_on_edges > nr_nodes then false
                else
                    let is_some_edge_updated = update_distances adjacency_list distances in
                    if complete_pass_on_edges = nr_nodes && is_some_edge_updated then true
                    else loop (complete_pass_on_edges + 1)
            in loop 1
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


let detect_negative_cycle edges nr_nodes =
    let graph = DirectedGraph.create_graph edges nr_nodes in
    DirectedGraph.has_negative_cycle graph


let () =
    let nr_nodes, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let result = detect_negative_cycle edges nr_nodes in
    print_int (if result then 1 else 0);
    print_newline()
