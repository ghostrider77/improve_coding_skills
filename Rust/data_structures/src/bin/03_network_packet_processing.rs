use std::collections::VecDeque;
use std::io;

#[derive(Clone, Debug)]
struct Packet {
    id: i32,
    arrival_time: i32,
    processing_time: i32,
}

#[derive(Debug)]
struct BufferedPacket {
    packet: Packet,
    finish_time: i32,
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_pair() -> (i32, i32) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_packet_info(nr_packets: i32) -> Vec<Packet> {
    let mut packets = Vec::with_capacity(nr_packets as usize);
    for id in 0..nr_packets {
        let (arrival_time, processing_time) = read_pair();
        packets.push(Packet {id, arrival_time, processing_time});
    }

    packets
}

fn remove_finished_packets(current_time: i32, buffer: &mut VecDeque<BufferedPacket>) -> Vec<BufferedPacket> {
    let mut removed_packets = Vec::new();
    loop {
        let Some(first_packet) = buffer.pop_front() else {
            return removed_packets
        };
        if first_packet.finish_time <= current_time {
            removed_packets.push(first_packet);

        } else {
            buffer.push_front(first_packet);
            return removed_packets
        }
    }
}

fn add_start_time_to_finished_packets(process_start_time: &mut Vec<i32>, packets: &[BufferedPacket]) {
    for BufferedPacket {packet: Packet {id, processing_time, ..}, finish_time} in packets {
        process_start_time[*id as usize] = finish_time - processing_time;
    }
}

fn process_packets(network_packets: &[Packet], max_buffer_size: usize, nr_packets: usize) -> Vec<i32> {
    let mut process_start_time = vec![0; nr_packets];
    let mut buffer = VecDeque::new();
    for packet @ Packet {id, arrival_time, processing_time} in network_packets {
        let finished_packets = remove_finished_packets(*arrival_time, &mut buffer);
        add_start_time_to_finished_packets(&mut process_start_time, &finished_packets);
        if buffer.len() >= max_buffer_size {
            process_start_time[*id as usize] = -1;

        } else {
            let next_packet = match buffer.back() {
                None => BufferedPacket {packet: packet.clone(), finish_time: arrival_time + processing_time},
                Some(last_packet) =>
                    BufferedPacket {packet: packet.clone(), finish_time: last_packet.finish_time + processing_time},
            };
            buffer.push_back(next_packet);
        }
    }

    add_start_time_to_finished_packets(&mut process_start_time, buffer.make_contiguous());
    process_start_time
}

fn main() {
    let (max_buffer_size, number_of_packets) = read_pair();
    let network_packets = read_packet_info(number_of_packets);
    let result = process_packets(&network_packets, max_buffer_size as usize, number_of_packets as usize);
    result.iter().for_each(|t| println!("{}", t));
}
