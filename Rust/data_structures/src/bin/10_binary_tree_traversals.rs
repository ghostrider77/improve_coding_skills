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

    fn preorder_traversal(&self) -> Vec<i32> {
        let mut keys = Vec::new();
        let mut stack = Vec::new();
        let mut node_ix = self.root_ix;
        loop {
            if node_ix != -1 {
                let node = &self.nodes[node_ix as usize];
                keys.push(node.key);
                node_ix = node.left_ix;
                stack.push(node.right_ix);
            } else {
                let Some(ix) = stack.pop() else {
                    return keys
                };
                node_ix = ix;
            }
        }
    }

    fn postorder_traversal(&self) -> Vec<i32> {
        let mut stack1 = Vec::new();
        let mut stack2 = Vec::new();
        let mut node_ix = self.root_ix;
        stack1.push(node_ix);
        loop {
            let Some(ix) = stack1.pop() else {
                break
            };
            node_ix = ix;
            if node_ix != -1 {
                stack2.push(node_ix);
                let node = &self.nodes[node_ix as usize];
                stack1.push(node.left_ix);
                stack1.push(node.right_ix);
            }
        }

        let mut keys = Vec::new();
        loop {
            let Some(ix) = stack2.pop() else {
                return keys
            };
            node_ix = ix;
            let node = &self.nodes[node_ix as usize];
            keys.push(node.key);
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

fn main() {
    let nr_nodes = read_line().trim().parse().unwrap();
    let nodes = read_nodes(nr_nodes);
    let tree = BinaryTree::new(nodes);
    let inorder = tree.inorder_traversal();
    let preorder = tree.preorder_traversal();
    let postorder = tree.postorder_traversal();
    println!("{}", inorder.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
    println!("{}", preorder.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
    println!("{}", postorder.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}
