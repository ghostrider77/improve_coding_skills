use std::{io, iter};

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn calc_prefix_function(text: &[char]) -> Vec<usize> {
    let n = text.len();
    let mut prefix_array = vec![0; n];
    let mut border = 0;
    for (ix, &chr) in text.iter().skip(1).enumerate() {
        while border > 0 && chr != text[border] {
            border = prefix_array[border - 1];
        }

        if chr == text[border] {
            border += 1;
        } else {
            border = 0;
        }

        prefix_array[ix + 1] = border;
    }

    prefix_array
}

fn find_pattern_in_text(text: &str, pattern: &str) -> Vec<usize> {
    let n = pattern.len();
    let chars = pattern.chars().chain(iter::once('$')).chain(text.chars()).collect::<Vec<_>>();
    let prefix_function = calc_prefix_function(&chars);

    let mut matching_indices = Vec::new();
    for (ix, &index) in prefix_function.iter().enumerate() {
        if ix > n && index == n {
            matching_indices.push(ix - 2*n);
        }
    }

    matching_indices
}

fn main() {
    let pattern = read_line().trim().to_string();
    let text = read_line().trim().to_string();
    let result = find_pattern_in_text(&text, &pattern);
    println!("{}", result.iter().map(|k| k.to_string()).collect::<Vec<_>>().join(" "));
}
