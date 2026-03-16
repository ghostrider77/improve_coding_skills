use std::io;

struct Node {
    key: i32,
    left_ix: i32,
    right_ix: i32,
}

struct BinaryTree {
    root_ix: i32,
    nodes: Vec<Node>,
}

impl BinaryTree {
    fn new(nodes: Vec<Node>) -> Self {
        BinaryTree { root_ix: 0, nodes }
    }

    fn inorder_traversal(&self) -> Vec<i32> {
        let mut keys = Vec::new();
        let mut stack = Vec::new();
        let mut node_ix = self.root_ix;
        loop {
            if node_ix != -1 {
                let node = &self.nodes[node_ix as usize];
                stack.push(node);
                node_ix = node.left_ix;
            } else {
                let Some(node) = stack.pop() else {
                    return keys
                };
                keys.push(node.key);
                node_ix = node.right_ix;
            }
        }
    }
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line
}

fn read_nodes(nr_nodes: usize) -> Vec<Node> {
    let mut nodes = Vec::with_capacity(nr_nodes);
    for _ in 0..nr_nodes {
        let line = read_line();
        let xs: Vec<i32> = convert_to_int_vector(&line);
        let [k, l, r] = xs.as_slice() else {
            panic!("Malformed input: {}", line);
        };
        nodes.push(Node { key: *k, left_ix: *l, right_ix: *r });
    }

    nodes
}

fn is_valid_binary_search_tree(tree: BinaryTree, nr_nodes: usize) -> bool {
    if nr_nodes <= 1 {
        return true
    }

    let keys = tree.inorder_traversal();
    keys.is_sorted()
}

fn main() {
    let nr_nodes = read_line().trim().parse().unwrap();
    let nodes = read_nodes(nr_nodes);
    let tree = BinaryTree::new(nodes);
    let result = is_valid_binary_search_tree(tree, nr_nodes);
    if result {
        println!("CORRECT");
    } else {
        println!("INCORRECT");
    }
}
