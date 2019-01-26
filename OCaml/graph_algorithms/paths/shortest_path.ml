open Batteries

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
        val create_graph : edge list -> int -> t
        val breadth_first_search : t -> int -> int array
    end

module Graph : GraphSig =
    struct
        type t = { adjacency_list : int list IntDict.t; nr_nodes : int }

        let reverse { left_end = a; right_end = b } = { left_end = b; right_end = a }

        let get_neighbours adjacency_list node =
            match IntDict.find_option adjacency_list node with
                | None -> []
                | Some neighbours -> neighbours

        let create_graph edges nr_nodes =
            let adjacency_list = IntDict.create 100 in
            let add_edge { left_end; right_end } =
                let neighbours = get_neighbours adjacency_list left_end in
                IntDict.replace adjacency_list left_end (right_end :: neighbours) in
            List.iter add_edge edges;
            List.iter (function e -> e |> reverse |> add_edge) edges;
            { adjacency_list; nr_nodes }

        let breadth_first_search { adjacency_list; nr_nodes } start_node =
            let distances = Array.make nr_nodes (-1) in
            distances.(start_node - 1) <- 0;
            let queue = Queue.create() in
            Queue.add start_node queue;
            while not (Queue.is_empty queue) do
                let node = Queue.pop queue in
                let neighbours = get_neighbours adjacency_list node in
                let update_distance neighbour =
                    if distances.(neighbour - 1) = -1 then
                        (Queue.add neighbour queue;
                         distances.(neighbour - 1) <- distances.(node - 1) + 1) in
                List.iter update_distance neighbours
            done;
            distances
    end


let read_two_ints() = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)


let read_edges nr_edges =
    let rec loop edges k =
        if k = nr_edges then edges
        else
            let left_end, right_end = read_two_ints() in
            loop ({ left_end; right_end } :: edges) (k + 1)
    in loop [] 0


let find_shortest_path edges nr_nodes start_node end_node =
    let graph = Graph.create_graph edges nr_nodes in
    let distances_from_start_node = Graph.breadth_first_search graph start_node in
    distances_from_start_node.(end_node - 1)


let () =
    let nr_nodes, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let start_node, end_node = read_two_ints() in
    let result = find_shortest_path edges nr_nodes start_node end_node in
    print_int (result);
    print_newline()
