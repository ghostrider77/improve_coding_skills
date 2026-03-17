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
        let mut node_indices = Vec::new();
        let mut stack = Vec::new();
        let mut node_ix = self.root_ix;
        loop {
            if node_ix != -1 {
                let node = &self.nodes[node_ix as usize];
                stack.push(node_ix);
                node_ix = node.left_ix;
            } else {
                let Some(ix) = stack.pop() else {
                    return node_indices
                };
                node_indices.push(ix);
                let node = &self.nodes[ix as usize];
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

fn are_there_duplicates_in_right_subtree(tree: BinaryTree, keys: &[i32], node_indices: &[i32]) -> bool {
    for (ix, node_index) in node_indices.iter().enumerate() {
        let node = &tree.nodes[*node_index as usize];
        if node.left_ix != -1 {
            let key_to_the_left = keys[ix - 1];
            if key_to_the_left == node.key {
                return false
            }
        }

    }

    true
}

fn is_valid_binary_search_tree(tree: BinaryTree, nr_nodes: usize) -> bool {
    if nr_nodes <= 1 {
        return true
    }

    let node_indices = tree.inorder_traversal();
    let keys = node_indices.iter().map(|ix| tree.nodes[*ix as usize].key).collect::<Vec<_>>();
    keys.is_sorted() && are_there_duplicates_in_right_subtree(tree, &keys, &node_indices)
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
