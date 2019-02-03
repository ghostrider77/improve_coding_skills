open Batteries

type point = { x : float; y: float }
type point_pair_distance = { index_of_p : int; index_of_q : int; distance : float }

module type UnionFindSig =
    sig
        type t
        val initialize : int -> t
        val find : t -> int -> int
        val union : t -> int -> int -> unit
    end

module UnionFind : UnionFindSig =
    struct
        type t = { parents : int array; ranks : int array }

        let change_parents_to_root parents indices_on_path root =
            List.iter (function ix -> parents.(ix) <- root) indices_on_path

        let initialize nr_points =
            { parents = Array.init nr_points (function ix -> ix); ranks = Array.make nr_points 0 }

        let find { parents } child_ix =
            let rec loop id parent_id indices_towards_root =
                if id = parent_id then id, indices_towards_root
                else loop parent_id parents.(parent_id) (id :: indices_towards_root) in
            let root, indices_on_path = loop child_ix parents.(child_ix) [] in
            change_parents_to_root parents indices_on_path root;
            root

        let union { parents; ranks } parent_index_p parent_index_q =
            if parent_index_p <> parent_index_q then
                if ranks.(parent_index_p) > ranks.(parent_index_q) then parents.(parent_index_q) <- parent_index_p
                else
                    parents.(parent_index_p) <- parent_index_q;
                    if ranks.(parent_index_p) = ranks.(parent_index_q)
                    then ranks.(parent_index_q) <- ranks.(parent_index_q) + 1
    end


let calc_distance { x = x1; y = y1 } { x = x2; y = y2 } = hypot (x1 -. x2) (y1 -. y2)


let read_two_floats() = Scanf.sscanf (read_line ()) "%f %f" (fun x y-> x, y)


let read_points nr_points =
    let rec loop acc k =
        if k = nr_points then acc
        else
            let x, y = read_two_floats() in
            loop ({ x; y } :: acc) (k + 1)
    in loop [] 0


let calc_pairwise_distances points =
    let distances =
        List.mapi
            (fun ix p ->
                List.mapi
                    (fun jy q -> { index_of_p = ix; index_of_q = ix + jy + 1; distance = calc_distance p q })
                    (List.drop (ix + 1) points))
        points in
    let comparison { distance = d1 } { distance = d2 } = Pervasives.compare d1 d2
    in List.sort comparison @@ List.concat distances


let calc_optimal_clustering nr_points points k =
    let distances = calc_pairwise_distances points in
    let clusters = UnionFind.initialize nr_points in
    let rec loop nr_clusters xs =
        let { index_of_p; index_of_q; distance } = List.hd xs in
        let cluster_of_p = UnionFind.find clusters index_of_p in
        let cluster_of_q = UnionFind.find clusters index_of_q in
        if cluster_of_p = cluster_of_q then loop nr_clusters (List.tl xs)
        else
            let () = UnionFind.union clusters cluster_of_p cluster_of_q in
            if nr_clusters = k then distance
            else loop (nr_clusters - 1) (List.tl xs)
    in loop nr_points distances


let () =
    let nr_points = read_int() in
    let points = read_points nr_points in
    let k = read_int() in
    let d = calc_optimal_clustering nr_points points k in
    print_float d;
    print_newline()
