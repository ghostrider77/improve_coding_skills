type edge = { left_end : int; right_end : int }

module IntSet = Set.Make(
    struct
        type t = int
        let compare = Pervasives.compare
end)

module IntDict = Hashtbl.Make(
    struct
        type t = int
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
end)

module type GraphSig =
    sig
        type t
        val create_graph : edge list -> t
        val get_neighbours : t -> int -> IntSet.t
    end

module Graph : GraphSig =
    struct
        type t = { adjacency_list : IntSet.t IntDict.t }

        let reverse { left_end = a; right_end = b } = { left_end = b; right_end = a }

        let get_neighbours { adjacency_list } node =
            match IntDict.find_opt adjacency_list node with
                | None -> IntSet.empty
                | Some neighbours -> neighbours

        let create_graph edges =
            let adjacency_list = IntDict.create 100 in
            let add_edge { left_end; right_end } =
                let neighbours = get_neighbours { adjacency_list } left_end in
                IntDict.replace adjacency_list left_end (IntSet.add right_end neighbours) in
            List.iter add_edge edges;
            List.iter (function e -> add_edge @@ reverse e) edges;
            { adjacency_list }
    end


let read_two_ints() = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)


let read_edges nr_edges =
    let rec loop edges k =
        if k = nr_edges then edges
        else
            let left_end, right_end = read_two_ints() in
            loop ({ left_end; right_end } :: edges) (k + 1)
    in loop [] 0


let get_unvisited_neighbours current_nodes graph visited_nodes =
    let collect_unvisited_neighbours node =
        let neighbours = Graph.get_neighbours graph node in
        IntSet.filter (function neighbour -> not (IntSet.mem neighbour visited_nodes)) neighbours in
    IntSet.fold (fun node acc -> IntSet.union acc (collect_unvisited_neighbours node)) current_nodes IntSet.empty


let are_nodes_connected edges start_node end_node =
    let graph = Graph.create_graph edges in
    let rec loop current_nodes visited_nodes =
        if IntSet.is_empty current_nodes then false
        else
            let unvisited_neighbours = get_unvisited_neighbours current_nodes graph visited_nodes in
            if IntSet.mem end_node unvisited_neighbours then true
            else loop unvisited_neighbours (IntSet.union visited_nodes unvisited_neighbours)
    in loop (IntSet.singleton start_node) (IntSet.singleton start_node)


let () =
    let _, nr_edges = read_two_ints() in
    let edges = read_edges nr_edges in
    let start_node, end_node = read_two_ints() in
    let result = are_nodes_connected edges start_node end_node in
    print_int (if result then 1 else 0);
    print_newline()
