use std::collections::{HashMap, HashSet, VecDeque};
use std::io;

#[derive(Clone, Eq, PartialEq)]
enum NodeColor {
    Red,
    Blue,
}

impl NodeColor {
    fn opposite(&self) -> Self {
        match self {
            NodeColor::Blue => NodeColor::Red,
            NodeColor::Red => NodeColor::Blue,
        }
    }
}

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

fn get_consistently_colored_component(graph: &Graph, start_node: i32) -> Option<HashSet<i32>> {
    let mut coloring = HashMap::from([(start_node, NodeColor::Red)]);
    let mut component = HashSet::from([start_node]);
    let mut queue = VecDeque::from([start_node]);
    while let Some(node) = queue.pop_front() {
        let node_color = coloring.get(&node).unwrap().clone();
        if let Some(neighbors) = graph.adjacency_list.get(&node) {
            for &neighbor in neighbors {
                if let Some(neighbor_color) = coloring.get(&neighbor) {
                    if node_color == *neighbor_color {
                        return None
                    }
                } else {
                    queue.push_back(neighbor);
                    coloring.insert(neighbor, node_color.opposite());
                    component.insert(neighbor);
                }
            }
        }
    }

    Some(component)
}

fn is_bipartite(graph: &Graph) -> bool {
    let mut unvisited_nodes = (1..=graph.nr_nodes).collect::<HashSet<_>>();
    while let Some(start_node) = unvisited_nodes.iter().next().cloned() {
        let Some(bipartite_component) = get_consistently_colored_component(graph, start_node) else {
            return false;
        };
        unvisited_nodes.retain(|x| !bipartite_component.contains(x));
    }

    true
}

fn main() {
    let (nr_nodes, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let graph = Graph::from_edge_list(nr_nodes, &edge_list);
    let result = is_bipartite(&graph);
    println!("{}", result as i32);
}
