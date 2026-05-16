use std::collections::{HashMap, HashSet, VecDeque};
use std::io;

struct Edge {
    start: i32,
    end: i32,
}

struct Graph {
    nr_nodes: i32,
    adjacency_list: HashMap<i32, HashSet<i32>>
}

impl Graph {
    fn from_edge_list(nr_nodes: i32, edges: &[Edge]) -> Self {
        let mut adjacency_list = HashMap::new();
        for &Edge { start, end } in edges {
            adjacency_list.entry(start).or_insert_with(HashSet::new).insert(end);
            adjacency_list.entry(end).or_insert_with(HashSet::new).insert(start);
        }

        Graph { nr_nodes, adjacency_list }
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

fn find_shortest_paths(graph: Graph, start_node: i32, target_node: i32) -> i32 {
    let mut distances = vec![-1; graph.nr_nodes as usize];
    distances[start_node as usize - 1] = 0;
    let mut queue = VecDeque::new();
    queue.push_back(start_node);
    while let Some(node) = queue.pop_front() {
        if let Some(neighbors) = graph.adjacency_list.get(&node) {
            for &neighbor in neighbors {
                if distances[neighbor as usize - 1] == -1 {
                    queue.push_back(neighbor);
                    distances[neighbor as usize - 1] = distances[node as usize - 1] + 1;
                }
            }
        }
    }

    distances[target_node as usize - 1]
}

fn main() {
    let (nr_nodes, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let (u, v) = read_pair();
    let graph = Graph::from_edge_list(nr_nodes, &edge_list);
    let result = find_shortest_paths(graph, u, v);
    println!("{}", result);
}
