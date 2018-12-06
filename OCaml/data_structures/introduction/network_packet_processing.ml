open Core_kernel

type packet = { id : int; arrival_time : int; processing_time : int }
type buffered_packet = { packet : packet; finish_time : int }


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let read_all_packets n =
    let rec read_packet acc k =
        if k = n then List.rev acc
        else
            let packet =
                Scanf.sscanf (read_line ()) "%d %d" (fun a p -> { id = k; arrival_time = a; processing_time = p }) in
            read_packet (packet :: acc) (k + 1)
    in read_packet [] 0


let remove_finished_packets_from_buffer current_time buffer =
    let rec remove removed_packets = match Dequeue.peek_front buffer with
        | None -> removed_packets
        | Some { finish_time } ->
            if finish_time <= current_time then
                let first_packet = Dequeue.dequeue_front_exn buffer in
                remove (first_packet :: removed_packets)
            else removed_packets
    in remove []


let add_start_time_to_finished_packets process_start_time finished_packets =
    let calc_start_time { packet = { id; processing_time }; finish_time } =
        process_start_time.(id) <- finish_time - processing_time in
    List.iter calc_start_time finished_packets


let process_packets network_packets max_buffer_size number_of_packets =
    let process_start_time = Array.make number_of_packets 0 in
    let buffer = Dequeue.create ~initial_length:max_buffer_size () in
    let insert_packet_to_buffer packet =
        let finished_packets = remove_finished_packets_from_buffer packet.arrival_time buffer in
        add_start_time_to_finished_packets process_start_time finished_packets;
        if Dequeue.is_empty buffer then
            Dequeue.enqueue_back buffer { packet; finish_time = packet.arrival_time + packet.processing_time }
        else if Dequeue.length buffer >= max_buffer_size then process_start_time.(packet.id) <- -1
        else
            let { finish_time } = Dequeue.peek_back_exn buffer in
            Dequeue.enqueue_back buffer { packet; finish_time = finish_time + packet.processing_time } in
    List.iter insert_packet_to_buffer network_packets;
    add_start_time_to_finished_packets process_start_time (Dequeue.to_list buffer);
    process_start_time


let () =
    let max_buffer_size, number_of_packets = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y) in
    let network_packets = read_all_packets number_of_packets in
    let process_start_time_of_packets = process_packets network_packets max_buffer_size number_of_packets in
    Array.iter (function s -> print_int s; print_newline()) process_start_time_of_packets
