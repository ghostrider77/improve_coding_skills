open Batteries

type point = { x : int; y: int }

module PointHash = Hashtbl.Make(
    struct
        type t = point
        let equal { x = x1; y = y1 } { x = x2; y = y2 } = x1 = x2 && y1 = y2
        let hash = Hashtbl.hash
    end)


let calc_distance { x = x1; y = y1 } { x = x2; y = y2 } = hypot (float (x1 - x2)) (float (y1 - y2))


let calc_minimum_spanning_tree points =
    let nodes_with_cost = PointHash.create 1000 in
    List.iter (function node -> PointHash.add nodes_with_cost node infinity) points;
    let starting_point = List.hd points in
    PointHash.replace nodes_with_cost starting_point 0.0;
    let rec loop total_cost =
        if PointHash.is_empty nodes_with_cost then total_cost
        else
            let v, cost_of_adding_v =
                PointHash.fold (fun node cost (min_node, min_cost) ->
                    if cost < min_cost then node, cost
                    else min_node, min_cost) nodes_with_cost ({ x = 0; y = 0 }, max_float) in
            PointHash.remove nodes_with_cost v;
            let update z cost_of_adding_z =
                let dist = calc_distance v z in
                if cost_of_adding_z > dist then PointHash.replace nodes_with_cost z dist in
            PointHash.iter update nodes_with_cost;
            loop (total_cost +. cost_of_adding_v)
    in loop 0.0


let read_two_ints() = Scanf.sscanf (read_line ()) "%d %d" (fun x y-> x, y)


let read_points nr_points =
    let rec loop acc k =
        if k = nr_points then acc
        else
            let x, y = read_two_ints() in
            loop ({ x; y } :: acc) (k + 1)
    in loop [] 0


let () =
    let nr_points = read_int() in
    let points = read_points nr_points in
    let result = calc_minimum_spanning_tree points in
    print_float result;
    print_newline()
