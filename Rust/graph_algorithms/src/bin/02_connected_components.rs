use std::collections::HashMap;
use std::io;

type Component = Vec<i32>;

struct Edge {
    start: i32,
    end: i32,
}

struct DFSState {
    previsit_numbers: Vec<i32>,
    postvisit_numbers: Vec<i32>,
    previsit_id: i32,
    postvisit_id: i32,
}

impl DFSState {
    fn new(nr_nodes: i32) -> Self {
        DFSState {
            previsit_numbers: vec![0; nr_nodes as usize],
            postvisit_numbers: vec![0; nr_nodes as usize],
            previsit_id: 1,
            postvisit_id: 1,
        }
    }

    fn is_visited(&self, node: i32) -> bool {
        self.previsit_numbers[node as usize - 1] > 0
    }
}

struct Graph {
    nr_nodes: i32,
    adjacency_list: HashMap<i32, Vec<i32>>
}

impl Graph {
    fn from_edge_list(nr_nodes: i32, edges: Vec<Edge>) -> Self {
        let mut adjacency_list = HashMap::new();
        for Edge { start, end } in edges {
            adjacency_list.entry(start).or_insert_with(Vec::new).push(end);
            adjacency_list.entry(end).or_insert_with(Vec::new).push(start);
        }

        Graph { nr_nodes, adjacency_list }
    }

    fn find_unvisited_neighbour_of_a_node(&self, state: &DFSState, node: i32) -> Option<i32> {
        match self.adjacency_list.get(&node) {
            None => None,
            Some(neighbors) =>
                neighbors
                    .iter()
                    .find(|neighbor| !state.is_visited(**neighbor))
                    .cloned()
        }
    }

    fn explore(&self, state: &mut DFSState, start_node: i32) -> Vec<i32> {
        state.previsit_numbers[start_node as usize - 1] = state.previsit_id;
        state.previsit_id += 1;
        let mut previsit_stack = vec![start_node];
        let mut current_component = vec![start_node];
        loop {
            let Some(last_node) = previsit_stack.pop() else {
                return current_component
            };
            match self.find_unvisited_neighbour_of_a_node(state, last_node) {
                None => {
                    state.postvisit_numbers[last_node as usize - 1] = state.postvisit_id;
                    state.postvisit_id += 1;
                },
                Some(neighbor) => {
                    state.previsit_numbers[neighbor as usize - 1] = state.previsit_id;
                    state.previsit_id += 1;
                    previsit_stack.push(last_node);
                    previsit_stack.push(neighbor);
                    current_component.push(neighbor);
                }
            };
        }
    }

    fn connected_components(&self) -> Vec<Component> {
        let mut state = DFSState::new(self.nr_nodes);
        let mut components = Vec::new();
        for starting_node in 1..=self.nr_nodes {
            if !state.is_visited(starting_node) {
                let current_component = self.explore(&mut state, starting_node);
                components.push(current_component);
            }
        }

        components
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

fn main() {
    let (nr_nodes, nr_edges) = read_pair();
    let edge_list = read_edges(nr_edges as usize);
    let graph = Graph::from_edge_list(nr_nodes, edge_list);
    let result = graph.connected_components();
    println!("{}", result.len());
}
