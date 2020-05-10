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


function preorder(tree::BinaryTree)
    keys = Int[]
    stack = Int[]
    nodeindex = tree.root
    while true
        if nodeindex != -1
            node = tree.nodes[nodeindex+1]
            push!(keys, node.key)
            nodeindex = node.left
            push!(stack, node.right)
        elseif !isempty(stack)
            nodeindex = pop!(stack)
        else
            return keys
        end
    end
end


function postorder(tree::BinaryTree)
    stack1 = Int[]
    stack2 = Int[]
    nodeindex = tree.root
    push!(stack1, nodeindex)
    while !isempty(stack1)
        nodeindex = pop!(stack1)
        if nodeindex != -1
            push!(stack2, nodeindex)
            node = tree.nodes[nodeindex+1]
            push!(stack1, node.left)
            push!(stack1, node.right)
        end
    end

    keys = Int[]
    while !isempty(stack2)
        nodeindex = pop!(stack2)
        node = tree.nodes[nodeindex+1]
        push!(keys, node.key)
    end
    keys
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


function main()
    nrnodes = parse(Int, readline())
    nodes = readnodes(nrnodes)
    tree = BinaryTree(nodes)
    println(join(inorder(tree), " "))
    println(join(preorder(tree), " "))
    println(join(postorder(tree), " "))
end


main()
