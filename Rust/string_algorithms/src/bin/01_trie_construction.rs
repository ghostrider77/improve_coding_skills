use std::collections::HashMap;
use std::io;

struct Trie {
    adjacency_list: HashMap<i32, Vec<(i32, char)>>
}

impl Trie {
    fn build(words: &[String]) -> Self {
        let mut node_counter = 0;
        let mut adjacency_list: HashMap<i32, Vec<(i32, char)>> = HashMap::new();
        for word in words {
            let mut current_node = 0;
            for character in word.chars() {
                let node_with_character_as_label =
                    adjacency_list
                        .get(&current_node)
                        .and_then(|ns| ns.iter().find(|(_, label)| *label == character).map(|(n, _)| *n));
                match node_with_character_as_label {
                    Some(node) => current_node = node,
                    None => {
                        node_counter += 1;
                        adjacency_list.entry(current_node).or_insert_with(Vec::new).push((node_counter, character));
                        current_node = node_counter;
                    }
                };
            }
        }

        Trie { adjacency_list }
    }
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn read_patterns(nr_patterns: i32) -> Vec<String> {
    let mut patterns = Vec::with_capacity(nr_patterns as usize);
    for _ in 0..nr_patterns {
        let pattern = read_line().trim().to_string();
        patterns.push(pattern);
    }

    patterns
}

fn main() {
    let nr_patterns = read_line().trim().parse().unwrap();
    let patterns = read_patterns(nr_patterns);
    let trie = Trie::build(&patterns);
    for (node, neighbors) in trie.adjacency_list {
        for (neighbor, label) in neighbors {
            println!("{}->{}:{}", node, neighbor, label);
        }
    }
}
