open Batteries

module type HashTableSig =
    sig
        type t
        val initialize : int -> int -> int -> t
        val add : t -> string -> unit
        val delete : t -> string -> unit
        val find : t -> string -> string
        val check : t -> int -> string
    end

module HashTable : HashTableSig =
    struct
        type chain = string list
        type t = { chains : chain array; prime : int; x : int; buckets : int }

        let initialize prime x nr_buckets =
            { chains = Array.make nr_buckets []; prime; x; buckets = nr_buckets }

        let polynomial_hashing s prime x nr_buckets =
            (String.fold_right (fun chr acc -> (acc * x + Char.code chr) mod prime) s 0) mod nr_buckets

        let add { chains = table; prime; x; buckets } word =
            let hash_value = polynomial_hashing word prime x buckets in
            let chain = table.(hash_value) in
            if not (List.mem word chain) then table.(hash_value) <- word :: chain

        let delete { chains = table; prime; x; buckets } word =
            let hash_value = polynomial_hashing word prime x buckets in
            let chain = table.(hash_value) in
            table.(hash_value) <- List.filter ((<>) word) chain

        let find { chains = table; prime; x; buckets } word =
            let hash_value = polynomial_hashing word prime x buckets in
            let chain = table.(hash_value) in
            if List.mem word chain then "yes" else "no"

        let check { chains = table } k = String.concat " " table.(k)
    end

type query =
    | Addition of string
    | Deletion of string
    | Find of string
    | Check of int
type parameters = { prime : int; x : int }


let convert_to_list = Str.(split @@ regexp " ")


exception Unknown_query_type


let read_queries nr_queries =
    let rec read_query acc k =
        if k = nr_queries then List.rev acc
        else
            let next_query = match convert_to_list @@ read_line() with
                | ["add"; word] -> Addition word
                | ["del"; word] -> Deletion word
                | ["find"; word] -> Find word
                | ["check"; bucket_id] -> Check (int_of_string bucket_id)
                | _ -> raise Unknown_query_type
            in read_query (next_query :: acc) (k + 1)
    in read_query [] 0


let process_queries queries nr_buckets { prime; x } =
    let hash_table = HashTable.initialize prime x nr_buckets in
    let rec loop acc = function
        | [] -> List.rev acc
        | Addition word :: qss ->
            HashTable.add hash_table word;
            loop acc qss
        | Deletion word :: qss ->
            HashTable.delete hash_table word;
            loop acc qss
        | Find word :: qss -> loop ((HashTable.find hash_table word) :: acc) qss
        | Check bucket :: qss -> loop ((HashTable.check hash_table bucket) :: acc) qss
    in loop [] queries


let () =
    let number_of_buckets = read_int() in
    let number_of_queries = read_int() in
    let queries = read_queries number_of_queries in
    let params = { prime = 1000000007; x = 263 } in
    let results = process_queries queries number_of_buckets params in
    List.iter print_endline results
