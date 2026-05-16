use std::collections::{HashMap, HashSet};
use std::io;

struct Edge {
    start: i32,
    end: i32,
}

struct Graph {
    adjacency_list: HashMap<i32, HashSet<i32>>
}

impl Graph {
    fn from_edge_list(edges: Vec<Edge>) -> Self {
        let mut adjacency_list = HashMap::new();
        for Edge { start, end } in edges {
            adjacency_list.entry(start).or_insert_with(HashSet::new).insert(end);
            adjacency_list.entry(end).or_insert_with(HashSet::new).insert(start);
        }

        Graph { adjacency_list }
    }
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_pair() -> (i32, i32) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_edges(nr_edges: usize) -> Vec<Edge> {
    let mut edges = Vec::with_capacity(nr_edges);
    for _ in 0..nr_edges {
        let (start, end) = read_pair();
        edges.push(Edge { start, end });
    }

    edges
}

fn are_nodes_connected(graph: Graph, u: i32, v: i32) -> bool {
    let mut visited_nodes = HashSet::new();
    let mut stack = vec![u];
    while let Some(node) = stack.pop() {
        if node == v {
            return true
        }

        if !visited_nodes.contains(&node) {
            visited_nodes.insert(node);
            if let Some(neighbors) = graph.adjacency_list.get(&node) {
                for neighbor in neighbors {
                    stack.push(*neighbor);
                }
            }
        }
    }

    false
}

fn main() {
    let (_, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let (u, v) = read_pair();
    let graph = Graph::from_edge_list(edge_list);
    let result = are_nodes_connected(graph, u, v);
    println!("{}", result as i32);
}
