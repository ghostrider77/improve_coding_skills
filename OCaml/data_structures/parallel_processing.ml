open Batteries

type processed_job = { thread_id: int; job_id : int; process_time : int; finish_time : int }
type job_schedule = { responsible_thread : int; start_time : int }


module MinHeap = Heap.Make(
    struct
        type t = processed_job
        let lt p q = p.finish_time < q.finish_time || (p.finish_time = q.finish_time && p.thread_id < q.thread_id)
        let compare p q =
            if p.thread_id = q.thread_id && p.finish_time = q.finish_time then 0
            else if lt p q then -1
            else 1
    end)


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let create_initial_heap jobs =
    List.fold_lefti (fun acc id pt ->
        MinHeap.insert acc { thread_id = id; job_id = id; process_time = pt; finish_time = pt }) MinHeap.empty jobs


let remove_remaining_elements_from_heap heap jobs_processed_by_threads =
    let rec remove hp =
        if MinHeap.size hp > 0 then
            let { thread_id; job_id; process_time; finish_time } = MinHeap.find_min hp in
            jobs_processed_by_threads.(job_id) <-
                { responsible_thread = thread_id; start_time = finish_time - process_time };
            remove (MinHeap.del_min hp)
    in remove heap


let process_jobs job_processing_times number_of_jobs number_of_threads =
    if number_of_jobs <= number_of_threads then
        Array.init number_of_jobs (function id -> { responsible_thread = id; start_time = 0 })
    else
        let jobs_processed_by_threads = Array.make number_of_jobs { responsible_thread = 0; start_time = 0 } in
        let initial_jobs, remaining_jobs = List.split_at number_of_threads job_processing_times in
        let rec loop acc job_id = function
            | [] -> acc
            | process_time :: t ->
                let { thread_id = free_thread_id;
                      job_id = finished_job_id;
                      process_time = finished_process_time;
                      finish_time = finished_end_time } = MinHeap.find_min acc in
                jobs_processed_by_threads.(finished_job_id) <-
                    { responsible_thread = free_thread_id; start_time = finished_end_time - finished_process_time };
                let next_job = { thread_id = free_thread_id; job_id; process_time;
                                 finish_time = finished_end_time + process_time } in
                let acc' = MinHeap.(acc |> del_min |> add next_job) in
                loop acc' (job_id + 1) t in
        let final_heap = loop (create_initial_heap initial_jobs) number_of_threads remaining_jobs in
        remove_remaining_elements_from_heap final_heap jobs_processed_by_threads;
        jobs_processed_by_threads


let () =
    let number_of_threads, number_of_jobs = (Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y)) in
    let job_processing_times = convert_to_intlist @@ read_line() in
    let result = process_jobs job_processing_times number_of_jobs number_of_threads in
    Array.iter (function { responsible_thread; start_time } ->
        print_endline @@ (string_of_int responsible_thread) ^ " " ^ (string_of_int start_time)) result
