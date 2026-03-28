use std::cmp::{Ordering, Reverse};
use std::collections::{BinaryHeap, HashMap, HashSet};
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

#[derive(Clone, Copy, Eq, PartialEq)]
enum Distance {
    Dist(i32),
    Infinity,
}

impl Distance {
    fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Distance::Dist(d1), Distance::Dist(d2)) => Distance::Dist(d1 + d2),
            _ => Distance::Infinity,
        }
    }
}

impl Ord for Distance {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Distance::Infinity, Distance::Infinity) => Ordering::Equal,
            (Distance::Infinity, Distance::Dist(_)) => Ordering::Greater,
            (Distance::Dist(_), Distance::Infinity) => Ordering::Less,
            (Distance::Dist(d1), Distance::Dist(d2)) => d1.cmp(d2),
        }
    }
}

impl PartialOrd for Distance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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

fn find_cheapest_path(graph: DirectedGraph, start: i32, end: i32) -> Distance {
    let mut distances = vec![Distance::Infinity; graph.nr_nodes as usize];
    distances[start as usize - 1] = Distance::Dist(0);
    let mut heap = BinaryHeap::from([Reverse((Distance::Dist(0), start))]);
    let mut finalized_nodes = HashSet::new();
    loop {
        let Some(Reverse((dist, node))) = heap.pop() else {
            return distances[end as usize - 1]
        };
        if !finalized_nodes.contains(&node) {
            if let Some(neighbors) = graph.adjacency_list.get(&node) {
                for &Edge {to, weight, ..} in neighbors {
                    let distance_through_node = dist.add(&Distance::Dist(weight));
                    if distances[to as usize - 1] > distance_through_node {
                        distances[to as usize - 1] = distance_through_node;
                        heap.push(Reverse((distance_through_node, to)));
                    }
                }
                finalized_nodes.insert(node);
            };
        };
    };
}

fn main() {
    let (nr_nodes, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let (s, t) = read_pair();
    let graph = DirectedGraph::from_edge_list(nr_nodes, edge_list);
    let result = find_cheapest_path(graph, s, t);
    match result {
        Distance::Infinity => println!("-1"),
        Distance::Dist(d) => println!("{}", d),
    };
}
