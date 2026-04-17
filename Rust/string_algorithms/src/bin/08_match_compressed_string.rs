use std::collections::{HashMap, HashSet};
use std::io;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn read_patterns(line: &str) -> Vec<String> {
    line
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

fn calc_first_occurrence_positions(text: &str) -> HashMap<char, i32> {
    let mut letter_counts = HashMap::new();
    for chr in text.chars() {
        letter_counts.entry(chr).and_modify(|cnt| *cnt += 1).or_insert(1);
    }

    let mut chars = letter_counts.keys().collect::<Vec<_>>();
    chars.sort();

    let mut first_occurrence = HashMap::new();
    let mut ix = 0;
    for &chr in chars {
        first_occurrence.insert(chr, ix);
        ix += letter_counts.get(&chr).unwrap();
    }

    first_occurrence
}

fn calc_count_matrix(text: &str, unique_characters: &HashSet<char>) -> HashMap<char, Vec<i32>> {
    let n = text.len();
    let mut count_matrix = HashMap::from_iter(unique_characters.iter().map(|&c| (c, vec![0; n + 1])));
    for (ix, current_chr) in text.chars().enumerate() {
        for chr in unique_characters {
            let counts = count_matrix.get_mut(chr).unwrap();
            counts[ix + 1] = if current_chr == *chr { counts[ix] + 1 } else { counts[ix] };
        }
    }
    count_matrix
}

fn pattern_matching(
    pattern: &str,
    last_column: &[char],
    first_occurrences: &HashMap<char, i32>,
    count_matrix: &HashMap<char, Vec<i32>>,
) -> usize {
    let mut top_pointer = 0;
    let mut bottom_pointer = last_column.len() - 1;
    for chr in pattern.chars().rev() {
        if !last_column[top_pointer..(bottom_pointer+1)].iter().any(|&c| c == chr) {
            return 0
        }
        let char_occurrence = first_occurrences.get(&chr).unwrap();
        let char_counter = count_matrix.get(&chr).unwrap();
        top_pointer = (char_occurrence + char_counter[top_pointer]) as usize;
        bottom_pointer = (char_occurrence + char_counter[bottom_pointer + 1] - 1) as usize;
    }

    bottom_pointer + 1 - top_pointer
}

fn improved_burrows_wheeler_pattern_matching(transformed_text: &str, patterns: &Vec<String>) -> Vec<usize> {
    let first_occurrences = calc_first_occurrence_positions(transformed_text);
    let unique_letters: HashSet<char> = HashSet::from_iter(first_occurrences.clone().into_keys());
    let count_matrix = calc_count_matrix(transformed_text, &unique_letters);

    let last_column = transformed_text.chars().collect::<Vec<_>>();
    let mut pattern_occurrences = Vec::new();
    for pattern in patterns {
        let nr_matches = pattern_matching(pattern, &last_column, &first_occurrences, &count_matrix);
        pattern_occurrences.push(nr_matches);
    }

    pattern_occurrences
}

fn main() {
    let transformed_text = read_line().trim().to_string();
    let _ = read_line();
    let patterns = read_patterns(&read_line());
    let result = improved_burrows_wheeler_pattern_matching(&transformed_text, &patterns);
    println!("{}", result.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(" "));
}
