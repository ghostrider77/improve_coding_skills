use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
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

fn update_distances(graph: &DirectedGraph, distances: &mut [Distance]) -> HashSet<i32> {
    let mut relaxed_nodes = HashSet::new();
    for (node, neighbors) in &graph.adjacency_list {
        if let dist @ Distance::Dist(_) = distances[*node as usize - 1] {
            for &Edge {to, weight, ..} in neighbors {
                let distance_through_node = dist.add(&Distance::Dist(weight));
                if distances[to as usize - 1] > distance_through_node {
                    distances[to as usize - 1] = distance_through_node;
                    relaxed_nodes.insert(to);
                }
            }
        }
    };
    relaxed_nodes
}

fn bellman_ford(graph: &DirectedGraph, start_node: i32) -> (Vec<Distance>, HashSet<i32>) {
    let mut distances = vec![Distance::Infinity; graph.nr_nodes as usize];
    distances[start_node as usize - 1] = Distance::Dist(0);
    for _ in 1..=graph.nr_nodes {
        update_distances(graph, &mut distances);
    }
    let relaxed_nodes = update_distances(graph, &mut distances);
    (distances, relaxed_nodes)
}

fn find_nodes_reachable_from_relaxed_nodes(graph: &DirectedGraph, relaxed_nodes: HashSet<i32>) -> HashSet<i32> {
    let mut visited_nodes = relaxed_nodes.clone();
    let mut queue = VecDeque::from_iter(relaxed_nodes);
    while let Some(node) = queue.pop_front() {
        if let Some(neighbors) = graph.adjacency_list.get(&node) {
            for &Edge {to, ..} in neighbors {
                if !visited_nodes.contains(&to) {
                    queue.push_back(to);
                    visited_nodes.insert(to);
                }
            }
        }
    }

    visited_nodes
}

fn find_shortest_paths(graph: DirectedGraph, start: i32) -> Vec<String> {
    let (distances, relaxed_nodes) = bellman_ford(&graph, start);
    let infinite_distance_nodes = find_nodes_reachable_from_relaxed_nodes(&graph, relaxed_nodes);
    let mut result = Vec::with_capacity(graph.nr_nodes as usize);
    for k in 1..=graph.nr_nodes {
        let distance_representation =
            if infinite_distance_nodes.contains(&k) { "-".to_string() }
            else {
                match distances[k as usize - 1] {
                    Distance::Infinity => "*".to_string(),
                    Distance::Dist(d) => d.to_string(),
                }
            };
        result.push(distance_representation);
    }

    result
}

fn main() {
    let (nr_nodes, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let start_node = read_line().trim().parse().unwrap();
    let graph = DirectedGraph::from_edge_list(nr_nodes, edge_list);
    let result = find_shortest_paths(graph, start_node);
    result.iter().for_each(|d| println!("{}", d));
}
