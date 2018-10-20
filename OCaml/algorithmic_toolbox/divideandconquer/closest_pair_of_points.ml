open Batteries

type point = { x : float; y : float }

let brute_force_size = 3


let convert_to_floatlist line = List.map float_of_string Str.(line |> split (regexp " "))


let distance { x = x'; y = y'} { x = x''; y = y''} = hypot (x' -. x'') (y' -. y'')


let read_points n =
    let rec read_point n acc = match n with
        | 0 -> List.rev acc
        | _ -> read_point (n - 1) ((Scanf.sscanf (read_line ()) "%f %f" (fun x y -> { x; y })) :: acc)
    in read_point n []


let calc_smallest_pairwise_distance points min_distance ~nr_points_compare_with =
    let calc_distances current_min_distance (p, ix) =
        let slice = List.take nr_points_compare_with (List.drop (ix + 1) points) in
        let update_min_distance acc q =
            let dist = distance p q
            in if dist < acc then dist else acc
        in List.fold_left update_min_distance current_min_distance slice in
    let zipped_with_index = List.mapi (fun ix elem -> (elem, ix)) points
    in List.fold_left calc_distances min_distance zipped_with_index


let calc_minimum_distance_in_stripe first second median delta =
    let in_stripe { x } = abs_float (x -. median) <= delta in
    let cmp_by_y { y=y' } { y=y'' } = compare y' y'' in
    let stripe = (List.filter in_stripe first) @ (List.filter in_stripe second)
    in calc_smallest_pairwise_distance (List.sort cmp_by_y stripe) delta ~nr_points_compare_with:7


let rec find_closest_points sorted_points n =
    if n <= brute_force_size then
        calc_smallest_pairwise_distance sorted_points max_float ~nr_points_compare_with:(brute_force_size - 1)
    else
        let middle_ix = n / 2 in
        let median_x = (List.at sorted_points middle_ix).x in
        let first, second = List.split_at middle_ix sorted_points in
        let delta1 = find_closest_points first middle_ix in
        let delta2 = find_closest_points second (n - middle_ix) in
        let delta = min delta1 delta2 in
        if (abs_float delta) < 1e-14 then 0.0
        else calc_minimum_distance_in_stripe first second median_x delta


let find_closest_pair_of_points points n =
    let cmp_by_x { x=x' } { x=x'' } = compare x' x'' in
    let sorted_points = List.sort cmp_by_x points
    in find_closest_points sorted_points n


let () =
    let n = read_int() in
    let points = read_points n in
    let dist = find_closest_pair_of_points points n in
    print_float dist;
    print_newline()
