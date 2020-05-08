using DataStructures


struct Packet
    id::Int
    arrivaltime::Int
    processingtime::Int
end


struct BufferedPacket
    packet::Packet
    finishtime::Int
end


converttointlist(line) = map(x -> parse(Int, x), split(line))


function readpacketinfo(nrpackets)
    packets = Packet[]
    for ix in 1:nrpackets
        tₐ , tₚ = readline() |> converttointlist
        push!(packets, Packet(ix, tₐ, tₚ))
    end
    packets
end


function removefinishedpackets(currenttime, buffer, size)
    removed = BufferedPacket[]
    while !isempty(buffer)
        firstpacket = popfirst!(buffer)
        size -= 1
        if firstpacket.finishtime <= currenttime
            push!(removed, firstpacket)
        else
            pushfirst!(buffer, firstpacket)
            size += 1
            return removed, size
        end
    end
    removed, size
end


function add_starttime_to_finishedpackets(starttime, finishedpackets)
    for bp in finishedpackets
        starttime[bp.packet.id] = bp.finishtime - bp.packet.processingtime
    end
end


function process(packets, maxbuffersize, nrpackets)
    starttime = zeros(Int, nrpackets)
    buffer = Deque{BufferedPacket}()
    size = 0
    for packet in packets
        finishedpackets, size = removefinishedpackets(packet.arrivaltime, buffer, size)
        add_starttime_to_finishedpackets(starttime, finishedpackets)
        if isempty(buffer)
            push!(buffer, BufferedPacket(packet, packet.arrivaltime+packet.processingtime))
            size += 1
        elseif size >= maxbuffersize
            starttime[packet.id] = -1
        else
            lastpacket = last(buffer)
            push!(buffer, BufferedPacket(packet, lastpacket.finishtime+packet.processingtime))
            size += 1
        end
    end
    add_starttime_to_finishedpackets(starttime, buffer)
    starttime
end


function main()
    maxbuffersize, nrpackets = readline() |> converttointlist
    packets = readpacketinfo(nrpackets)
    starttimes = process(packets, maxbuffersize, nrpackets)
    println(join(starttimes, "\n"))
end


main()
