use std::collections::HashMap;
use std::io;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn create_index_column(chrs: &Vec<char>) -> Vec<(char, i32)> {
    let mut counts = HashMap::new();
    let mut indexed_chars = Vec::with_capacity(chrs.len());
    for &chr in chrs {
        let index = *counts.entry(chr).and_modify(|cnt| *cnt += 1).or_insert(0);
        indexed_chars.push((chr, index));
    }

    indexed_chars
}

fn inverse_burrows_wheeler(transformed: &str) -> String {
    let chars = transformed.chars().collect::<Vec<_>>();
    let mut sorted_chars = chars.clone();
    sorted_chars.sort();
    let last_column = create_index_column(&chars);
    let first_column: HashMap<(char, i32), usize> =
        HashMap::from_iter(create_index_column(&sorted_chars).into_iter().enumerate().map(|(p, ix)| (ix, p)));

    let mut first_column_position = 0;
    let mut string = Vec::with_capacity(chars.len());
    for _ in 0..chars.len() {
        let letter_count @ (chr, _) = last_column[first_column_position];
        string.push(chr);
        first_column_position = *first_column.get(&letter_count).unwrap();
    }
    let mut restored = (0..(chars.len() - 1)).rev().map(|ix| string[ix]).collect::<Vec<_>>();
    restored.push('$');
    restored.into_iter().collect()
}

fn main() {
    let transformed_text = read_line().trim().to_string();
    let result = inverse_burrows_wheeler(&transformed_text);
    println!("{}", result);
}
