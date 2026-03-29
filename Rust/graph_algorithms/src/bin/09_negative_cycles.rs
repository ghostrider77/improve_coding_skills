use std::collections::HashMap;
use std::io;

#[derive(Clone)]
struct Edge {
    from: i32,
    to: i32,
    weight: i32,
}

struct DirectedGraph {
    nr_nodes: i32,
    adjacency_list: HashMap<i32, Vec<Edge>>
}

impl DirectedGraph {
    fn from_edge_list(nr_nodes: i32, edges: Vec<Edge>) -> Self {
        let mut adjacency_list = HashMap::new();
        for edge @ Edge { from, ..} in edges {
            adjacency_list.entry(from).or_insert_with(Vec::new).push(edge);
        }

        DirectedGraph { nr_nodes, adjacency_list }
    }
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn read_pair() -> (i32, i32) {
    let line = read_line();
    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_edges(nr_edges: usize) -> Vec<Edge> {
    let mut edges = Vec::with_capacity(nr_edges);
    for _ in 0..nr_edges {
        let line = read_line();
        let xs: Vec<i32> = convert_to_int_vector(&line);
        let [a, b, w] = xs.as_slice() else {
            panic!("Malformed input: {}", line);
        };
        edges.push(Edge { from: *a, to: *b, weight: *w });
    }

    edges
}

fn update_distances(graph: &DirectedGraph, distances: &mut [i32]) -> bool {
    let mut any_node_updated = false;
    for (node, neighbors) in &graph.adjacency_list {
        let dist_node = distances[*node as usize - 1];
        for &Edge {to, weight, ..} in neighbors {
            let distance_through_node = dist_node + weight;
            if distances[to as usize - 1] > distance_through_node {
                distances[to as usize - 1] = distance_through_node;
                any_node_updated = true
            }
        }
    };
    any_node_updated
}

fn has_negative_cycle(graph: DirectedGraph) -> bool {
    let n = graph.nr_nodes as usize;
    let mut distances = vec![0; n];
    let mut complete_pass_on_edges = 1;
    while complete_pass_on_edges <= n {
        let has_anything_updated = update_distances(&graph, &mut distances);
        if complete_pass_on_edges == n && has_anything_updated {
            return true
        }
        complete_pass_on_edges += 1;
    }
    false
}

fn main() {
    let (nr_nodes, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let graph = DirectedGraph::from_edge_list(nr_nodes, edge_list);
    let result = has_negative_cycle(graph);
    println!("{}", result as i32);
}
