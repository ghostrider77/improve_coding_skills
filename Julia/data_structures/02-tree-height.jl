struct Node
    key::Int
    children::Set{Int}

    function Node(key::Int, children::Set{Int}=Set{Int}())
        new(key, children)
    end
end


function addchild!(node::Node, child::Int)
    push!(node.children, child)
end


struct Tree
    root::Node
    nodes::Vector{Node}
end


converttointlist(line) = map(x -> parse(Int, x), split(line))


function buildtree(nrnodes, nodeparents)
    nodes = map(k -> Node(k), 1:nrnodes)
    root = 0
    for (nodeid, parentid) in enumerate(nodeparents)
        if parentid == -1
            root = nodes[nodeid]
        else
            addchild!(nodes[parentid+1], nodeid)
        end
    end
    Tree(root, nodes)
end


function childrenofnodes(keys, nodes)
    children = Set()
    for key in keys
        union!(children, nodes[key].children)
    end
    children
end


function treedepth(tree)
    keys = Set([tree.root.key])
    depth = 0
    while !isempty(keys)
        depth += 1
        keys = childrenofnodes(keys, tree.nodes)
    end
    depth
end


function main()
    nrnodes = parse(Int, readline())
    parentids = readline() |> converttointlist
    tree = buildtree(nrnodes, parentids)
    depth = treedepth(tree)
    println(depth)
end


main()
