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

    fn match_text_with_patterns(&self, text: &str) -> bool {
        let mut current_node = 0;
        for chr in text.chars() {
            let Some(next_node) =
                self.adjacency_list
                    .get(&current_node)
                    .and_then(|ns| ns.iter().find(|(_, label)| *label == chr).map(|(n, _)| *n)) else { return false };

            if !self.adjacency_list.contains_key(&next_node) {
                return true

            } else {
                current_node = next_node;
            }
        }

        false
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

fn multiple_pattern_matching(text: String, patterns: Vec<String>) -> Vec<usize> {
    let trie = Trie::build(&patterns);
    let mut indices = Vec::new();
    for ix in 0..text.len() {
        if trie.match_text_with_patterns(&text[ix..]) {
            indices.push(ix);
        }
    }

    indices
}

fn main() {
    let text = read_line().trim().to_string();
    let nr_patterns = read_line().trim().parse().unwrap();
    let patterns = read_patterns(nr_patterns);
    let result = multiple_pattern_matching(text, patterns);
    println!("{}", result.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(" "));
}
