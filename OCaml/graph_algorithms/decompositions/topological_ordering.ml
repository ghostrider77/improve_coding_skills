open Batteries

type edge = { left_end : int; right_end : int }
type component = int list
type dfs_output = { components : component list; topological_ordering : int list }

module IntMap = Map.Make(
    struct
        type t = int
        let compare = Pervasives.compare
    end)

module type DirectedGraphSig =
    sig
        type t
        val create_graph : edge list -> int -> t
        val depth_first_search : t -> dfs_output
    end

module DirectedGraph : DirectedGraphSig =
    struct
        type t = { adjacency_list : int list IntMap.t; nr_nodes : int }

        let range a b =
            let rec loop acc left =
                if left >= b then List.rev acc
                else loop (left :: acc) (left + 1)
            in loop [] a

        let get_neighbours adjacency_list node =
            if IntMap.mem node adjacency_list then IntMap.find node adjacency_list
            else []

        let create_graph edges nr_nodes =
            let add_edge { left_end; right_end } adjacency_list =
                let neighbours = get_neighbours adjacency_list left_end in
                IntMap.add left_end (right_end :: neighbours) adjacency_list in
            let adjacency_list = List.fold_left (fun acc edge -> add_edge edge acc) IntMap.empty edges
            in { adjacency_list; nr_nodes }

        let ordered_nodes nr_nodes = range 1 (nr_nodes + 1)

        let depth_first_search { adjacency_list; nr_nodes } =
            let visit_started = Array.make nr_nodes 0 in
            let visit_ended = Array.make nr_nodes 0 in
            let topological_sorting = DynArray.create() in
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
                            DynArray.add topological_sorting node;
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
            let components = find_components (range 1 (nr_nodes + 1)) [] in
            { components; topological_ordering = List.rev @@ DynArray.to_list topological_sorting }
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
    let graph = DirectedGraph.create_graph edges nr_nodes in
    let { topological_ordering } = DirectedGraph.depth_first_search graph in
    List.iter (function x -> print_int x; print_string " ") topological_ordering;
    print_newline()
