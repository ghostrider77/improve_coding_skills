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
        val bellman_ford : t -> int -> float array * int list
        val find_nodes_reachable_from_relaxed_nodes : t -> int list -> IntSet.t
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

        let update_distances adjacency_list distances ~collect_relaxed_nodes =
            let update node neighbours acc =
                let dist_node = distances.(node - 1) in
                if dist_node = infinity then acc
                else
                    let traverse inner_acc { tip = neighbour; edge_weight = weight } =
                        let distance_through_node = dist_node +. float weight in
                        if distances.(neighbour - 1) > distance_through_node then
                            (distances.(neighbour - 1) <- distance_through_node;
                             neighbour :: inner_acc)
                        else inner_acc in
                    List.fold_left traverse acc neighbours in
            let updated_nodes = IntMap.fold update adjacency_list [] in
            if collect_relaxed_nodes then updated_nodes else []

        let bellman_ford { adjacency_list; nr_nodes } start_node =
            let distances = Array.make nr_nodes infinity in
            distances.(start_node - 1) <- 0.0;
            let rec loop pass_on_edges =
                if pass_on_edges < nr_nodes then
                    let _ = update_distances adjacency_list distances ~collect_relaxed_nodes:false in
                    loop (pass_on_edges + 1)
                else update_distances adjacency_list distances ~collect_relaxed_nodes:true in
            let relaxed_nodes = loop 1
            in distances, relaxed_nodes

        let find_nodes_reachable_from_relaxed_nodes { adjacency_list; nr_nodes } relaxed_nodes =
            let queue = Queue.create() in
            List.iter (function node -> Queue.add node queue) relaxed_nodes;
            let rec loop visited_nodes =
                if Queue.is_empty queue then visited_nodes
                else
                    let node = Queue.pop queue in
                    let neighbours = get_neighbours adjacency_list node in
                    let traverse acc { tip = neighbour; edge_weight = weight } =
                        if not @@ IntSet.mem neighbour visited_nodes then
                            (Queue.add neighbour queue;
                             IntSet.add neighbour acc)
                        else acc in
                    let more_nodes = List.fold_left traverse IntSet.empty neighbours in
                    loop @@ IntSet.union visited_nodes more_nodes
            in loop @@ IntSet.of_list relaxed_nodes

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


let calc_shortest_paths edges nr_nodes start_node =
    let graph = DirectedGraph.create_graph edges nr_nodes in
    let distances, relaxed_nodes = DirectedGraph.bellman_ford graph start_node in
    let infinite_distance_nodes = DirectedGraph.find_nodes_reachable_from_relaxed_nodes graph relaxed_nodes in
    IntSet.iter (function node -> distances.(node - 1) <- neg_infinity) infinite_distance_nodes;
    Array.to_list distances


let convert_distance_to_symbol dist =
    if abs_float dist <> infinity then dist |> int_of_float |> string_of_int
    else if dist > 0.0 then "*"
    else "-"


let () =
    let nr_nodes, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let start_node = read_int() in
    let distances = calc_shortest_paths edges nr_nodes start_node in
    List.iter (function d -> convert_distance_to_symbol d |> print_endline) distances
