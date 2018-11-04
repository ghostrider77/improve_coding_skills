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

module type TreeSig =
    sig
        type t
		val build_tree : int -> int list -> t
        val calc_depth : t -> int
    end

module Tree : TreeSig =
    struct
        type node = { key : int; children : IntSet.t }
        type t = { root : node; nodes : node array }

		exception Root_is_not_unique

		let get_index_of_root children_of_nodes =
			match IntSet.elements (IntDict.find children_of_nodes (-1)) with
				| [ix] -> ix
				| _ -> raise Root_is_not_unique

		let get_or_else key dict default =
			if IntDict.mem dict key then IntDict.find dict key
			else default

        let build_tree nr_nodes parents_of_nodes =
			let table = IntDict.create nr_nodes in
			let update node_id parent_id =
				let children = get_or_else parent_id table IntSet.empty in
				let children' = IntSet.add node_id children in
				IntDict.replace table parent_id children' in
			List.iteri update parents_of_nodes;
			let nodes = Array.init nr_nodes (function k -> { key = k; children = get_or_else k table IntSet.empty }) in
			let index_of_root = get_index_of_root table
			in { root = Array.get nodes index_of_root; nodes }

        let get_children_of_nodes keys nodes =
			let extract_children key = match Array.get nodes key with
				| { children } -> children in
			IntSet.fold (fun key acc -> IntSet.union acc (extract_children key)) keys IntSet.empty

        let calc_depth { root; nodes } =
			let rec loop depth keys =
				if IntSet.is_empty keys then depth
				else
					let next_keys = get_children_of_nodes keys nodes in
					loop (depth + 1) next_keys
			in loop 0 (IntSet.singleton root.key)
    end


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let () =
    let number_of_nodes = read_int() in
    let parents_of_nodes = convert_to_intlist (read_line()) in
	let tree = Tree.build_tree number_of_nodes parents_of_nodes in
	let depth = Tree.calc_depth tree in
	print_int depth;
	print_newline()
