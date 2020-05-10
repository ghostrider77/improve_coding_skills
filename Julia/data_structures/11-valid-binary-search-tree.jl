struct Node
    key::Int
    left::Int
    right::Int
end


struct BinaryTree
    root::Int
    nodes::Vector{Node}

    BinaryTree(nodes::Vector{Node}) = new(0, nodes)
end


function inorder(tree::BinaryTree)
    keys = Int[]
    stack = Node[]
    nodeindex = tree.root
    while true
        if nodeindex != -1
            node = tree.nodes[nodeindex+1]
            push!(stack, node)
            nodeindex = node.left
        elseif !isempty(stack)
            node = pop!(stack)
            push!(keys, node.key)
            nodeindex = node.right
        else
            return keys
        end
    end
end


converttointlist(line) = map(x -> parse(Int, x), split(line))


function readnodes(nrnodes)
    nodes = Node[]
    for _ in 1:nrnodes
        line = readline() |> converttointlist
        push!(nodes, Node(line...))
    end
    nodes
end


function validsearchtree(tree, nrnodes)
    if nrnodes <= 1
        true
    else
        keys = inorder(tree)
        issorted(keys)
    end
end


function main()
    nrnodes = parse(Int, readline())
    nodes = readnodes(nrnodes)
    tree = BinaryTree(nodes)
    println(validsearchtree(tree, nrnodes) ? "CORRECT" : "INCORRECT")
end


main()
