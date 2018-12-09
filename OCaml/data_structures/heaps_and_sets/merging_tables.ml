open Batteries

module type UnionFindSig =
    sig
        type t
        val initialize : int -> int list -> t
        val union : t -> int -> int -> int
    end

module UnionFind : UnionFindSig =
    struct
        type t = { table_rows : int array; parents : int array; ranks : int array }

        let largest_table_size = ref 0

        let change_parents_to_root parents indices_on_path root =
            List.iter (function ix -> parents.(ix) <- root) indices_on_path

        let initialize nr_tables initial_table_rows =
            largest_table_size := List.max initial_table_rows;
            { table_rows = Array.of_list initial_table_rows;
              parents = Array.init nr_tables (function ix -> ix);
              ranks = Array.make nr_tables 0 }

        let find parents child_ix =
            let rec loop id parent_id indices_towards_root =
                if id = parent_id then id, indices_towards_root
                else loop parent_id parents.(parent_id) (id :: indices_towards_root) in
            let root, indices_on_path = loop child_ix parents.(child_ix) [] in
            change_parents_to_root parents indices_on_path root;
            root

        let union { table_rows; parents; ranks } source destination =
            let source_id = find parents source in
            let dest_id = find parents destination in
            if source_id = dest_id then !largest_table_size
            else if ranks.(source_id) > ranks.(dest_id) then
                let () =
                    (parents.(dest_id) <- source_id;
                     table_rows.(source_id) <- table_rows.(source_id) + table_rows.(dest_id);
                     table_rows.(dest_id) <- 0;
                     largest_table_size := max !largest_table_size table_rows.(source_id))
                in !largest_table_size
            else
                let () =
                    (parents.(source_id) <- dest_id;
                     table_rows.(dest_id) <- table_rows.(dest_id) + table_rows.(source_id);
                     table_rows.(source_id) <- 0;
                     if ranks.(source_id) = ranks.(dest_id) then ranks.(dest_id) <- ranks.(dest_id) + 1;
                     largest_table_size := max !largest_table_size table_rows.(dest_id))
                in !largest_table_size
    end

type table_operation = { destination : int; source : int }


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let read_table_operations nr_operations =
    let rec read_one_line acc k =
        if k = nr_operations then List.rev acc
        else
            let d, s = Scanf.sscanf (read_line()) "%d %d" (fun x y -> x, y) in
            read_one_line ({ destination = d - 1; source = s - 1 } :: acc) (k + 1)
    in read_one_line [] 0


let process_merge_requests table_rows nr_tables operations =
    let tables = UnionFind.initialize nr_tables table_rows in
    List.map (function { destination; source } -> UnionFind.union tables source destination ) operations


let () =
    let nr_tables, nr_operations = Scanf.sscanf (read_line()) "%d %d" (fun x y -> x, y) in
    let table_rows = convert_to_intlist @@ read_line() in
    let operations = read_table_operations nr_operations in
    let results = process_merge_requests table_rows nr_tables operations in
    List.iter (function n -> print_int n; print_newline()) results
