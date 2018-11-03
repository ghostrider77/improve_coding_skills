import sys
from collections import namedtuple, deque

Packet = namedtuple("Packet", ["id", "arrival_time", "processing_time"])
BufferedPacket = namedtuple("BufferedPacket", ["packet", "finish_time"])


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_packet_info(reader, number_of_packets):
    return (Packet(ix, *convert_to_intlist(next(reader))) for ix in range(number_of_packets))


def remove_finished_packets_from_buffer(current_time, buffer, buffer_size):
    removed_packets = []
    while buffer:
        first_packet = buffer.popleft()
        buffer_size -= 1
        if first_packet.finish_time <= current_time:
            removed_packets.append(first_packet)
        else:
            buffer.appendleft(first_packet)
            buffer_size += 1
            return removed_packets, buffer_size
    return removed_packets, buffer_size


def add_start_time_to_finished_packets(process_start_time, finished_packets):
    for (packet_id, _, process_time), finish_time in finished_packets:
        process_start_time[packet_id] = finish_time - process_time


def process_packets(network_packets, max_buffer_size, number_of_packets):
    process_start_time = [0] * number_of_packets
    buffer = deque(maxlen=max_buffer_size)
    buffer_size = 0
    for packet in network_packets:
        finished_packets, buffer_size = remove_finished_packets_from_buffer(packet.arrival_time, buffer, buffer_size)
        add_start_time_to_finished_packets(process_start_time, finished_packets)
        if not buffer:
            buffer.append(BufferedPacket(packet=packet, finish_time=packet.arrival_time+packet.processing_time))
            buffer_size += 1
        elif buffer_size >= max_buffer_size:
            process_start_time[packet.id] = -1
        else:
            last_packet = buffer[-1]
            buffer.append(BufferedPacket(packet=packet, finish_time=last_packet.finish_time+packet.processing_time))
            buffer_size += 1
    add_start_time_to_finished_packets(process_start_time, buffer)
    return process_start_time


def main():
    reader = sys.stdin
    max_buffer_size, number_of_packets = convert_to_intlist(next(reader))
    network_packets = read_packet_info(reader, number_of_packets)
    process_start_time_of_packets = process_packets(network_packets, max_buffer_size, number_of_packets)
    for time in process_start_time_of_packets:
        print(time)


if __name__ == "__main__":
    main()
