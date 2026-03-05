use std::collections::HashSet;
use std::io;

#[derive(Clone, Debug)]
struct Node {
    key: usize,
    children: HashSet<usize>,
}

impl Node {
    fn new(key: usize) -> Self {
        Self {key, children: HashSet::new()}
    }

    fn add_child(&mut self, child: usize) {
        self.children.insert(child);
    }
}

#[derive(Debug)]
struct Tree {
    root: Node,
    nodes: Vec<Node>,
}

impl Tree {
    fn create(nr_nodes: i32, parents_of_nodes: &[i32]) -> Self {
        let mut nodes: Vec<Node> = (0..nr_nodes).map(|k| Node::new(k as usize)).collect();
        for (node_id, parent_id) in parents_of_nodes.iter().enumerate() {
            if *parent_id != -1 {
                nodes[*parent_id as usize].add_child(node_id);
            }
        }
        let Some((root_ix, _)) = parents_of_nodes.iter().enumerate().find(|(_, parent_id)| **parent_id == -1) else {
            panic!("No root node has found.");
        };
        let root = nodes[root_ix].clone();
        Tree {root, nodes}
    }

    fn get_depth(&self) -> i32 {
        let mut keys_at_level = vec![self.root.key];
        let mut depth = 0;
        while !keys_at_level.is_empty() {
            keys_at_level =
                keys_at_level
                    .iter()
                    .flat_map(|key| self.nodes[*key].children.iter().cloned())
                    .collect();
            depth += 1;
        };

        depth
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

fn main() {
    let n: i32 = read_line().trim().parse().unwrap();
    let parent_ids = convert_to_int_vector(&read_line());
    let tree = Tree::create(n, &parent_ids);
    let result = tree.get_depth();
    println!("{}", result);
}
