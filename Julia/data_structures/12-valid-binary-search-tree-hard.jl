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
    nodeindices = Int[]
    stack = Int[]
    nodeindex = tree.root
    while true
        if nodeindex != -1
            node = tree.nodes[nodeindex+1]
            push!(stack, nodeindex)
            nodeindex = node.left
        elseif !isempty(stack)
            ix = pop!(stack)
            node = tree.nodes[ix+1]
            push!(nodeindices, ix)
            nodeindex = node.right
        else
            return nodeindices
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


function duplicates_in_rightsubtree(keys, nodeindices, tree)
    for (ix, nodeindex) in enumerate(nodeindices)
        node = tree.nodes[nodeindex+1]
        if node.left != -1
            keytoleft = keys[ix-1]
            if keytoleft == node.key
                return false
            end
        end
    end
    true
end


function validsearchtree(tree, nrnodes)
    if nrnodes <= 1
        true
    else
        nodeindices = inorder(tree)
        keys = map(ix -> tree.nodes[ix+1].key, nodeindices)
        issorted(keys) && duplicates_in_rightsubtree(keys, nodeindices, tree)
    end
end


function main()
    nrnodes = parse(Int, readline())
    nodes = readnodes(nrnodes)
    tree = BinaryTree(nodes)
    println(validsearchtree(tree, nrnodes) ? "CORRECT" : "INCORRECT")
end


main()
