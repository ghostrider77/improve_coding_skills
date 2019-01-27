open Batteries

type edge = { left_end : int; right_end : int }
type node_color = Red | Blue | Unfilled

module IntDict = Hashtbl.Make(
    struct
        type t = int
        let equal = Pervasives.(=)
        let hash = Hashtbl.hash
    end)

module IntSet = Set.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module type GraphSig =
    sig
        type t
        val create_graph : edge list -> int -> t
        val is_bipartite : t -> bool
    end

module Graph : GraphSig =
    struct
        type t = { adjacency_list : int list IntDict.t; nr_nodes : int }

        let reverse { left_end = a; right_end = b } = { left_end = b; right_end = a }

        let range a b =
            let rec loop acc left =
                if left >= b then List.rev acc
                else loop (left :: acc) (left + 1)
            in loop [] a

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

        let is_bipartite { adjacency_list; nr_nodes}  =
            let coloring = Array.make nr_nodes Unfilled in
            let is_visited node = coloring.(node - 1) <> Unfilled in
            let get_consistently_colored_components start_node =
                coloring.(start_node - 1) <- Red;
                let queue = Queue.create() in
                Queue.add start_node queue;
                let rec color_neighbours neighbours node_color component = match neighbours with
                    | [] -> (component, true)
                    | neighbour :: rest ->
                        if is_visited neighbour then
                            if node_color = coloring.(neighbour - 1) then (component, false)
                            else color_neighbours rest node_color component
                        else
                            let () = Queue.add neighbour queue in
                            coloring.(neighbour - 1) <- (if node_color = Red then Blue else Red);
                            let component' = IntSet.add neighbour component in
                            color_neighbours rest node_color component' in
                let rec loop component =
                    if Queue.is_empty queue then Some component
                    else
                        let node = Queue.pop queue in
                        let node_color = coloring.(node - 1) in
                        let neighbours = get_neighbours adjacency_list node in
                        let component', verdict = color_neighbours neighbours node_color component in
                        if verdict then loop component' else None
            in loop (IntSet.singleton start_node) in
            let rec traverse_components unvisited_nodes =
                if IntSet.is_empty unvisited_nodes then true
                else
                    let start_node = IntSet.any unvisited_nodes in
                    match get_consistently_colored_components start_node with
                        | None -> false
                        | Some bipartite_component ->
                            let remaining_nodes = IntSet.diff unvisited_nodes bipartite_component in
                            traverse_components remaining_nodes
            in traverse_components @@ IntSet.of_list (range 1 (nr_nodes + 1))
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
    let result = Graph.is_bipartite graph in
    print_int (if result then 1 else 0);
    print_newline()
