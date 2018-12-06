type query =
    | Addition of int * string
    | Deletion of int
    | Find of int


let max_size = 10000000


let convert_to_list = Str.(split @@ regexp " ")


exception Unknown_query_type


let read_queries nr_queries =
    let rec read_query acc k =
        if k = nr_queries then List.rev acc
        else
            let next_query = match convert_to_list @@ read_line() with
                | ["add"; number; name] -> Addition (int_of_string number, name)
                | ["del"; number] -> Deletion (int_of_string number)
                | ["find"; number] -> Find (int_of_string number)
                | _ -> raise Unknown_query_type
            in read_query (next_query :: acc) (k + 1)
    in read_query [] 0


let process_queries queries =
    let phone_book = Array.make max_size None in
    let rec loop acc = function
        | [] -> List.rev acc
        | Addition (number, name) :: qss ->
            phone_book.(number) <- Some name;
            loop acc qss
        | Deletion number :: qss ->
            phone_book.(number) <- None;
            loop acc qss
        | Find number :: qss -> match phone_book.(number) with
            | None -> loop ("not found" :: acc) qss
            | Some name -> loop (name :: acc) qss
    in loop [] queries


let () =
    let nr_queries = read_int() in
    let queries = read_queries nr_queries in
    let results = process_queries queries in
    List.iter print_endline results
