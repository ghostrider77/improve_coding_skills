use std::cmp::Ordering;
use std::io;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn calc_suffix_array(text: &str) -> Vec<usize> {
    let chars = text.chars().collect::<Vec<_>>();
    let n = chars.len();
    let mut indices = (0..n).collect::<Vec<_>>();
    indices.sort_by(|i, j| {
        let limit = (n - i).min(n - j);
        for k in 0..limit {
            if chars[i + k] < chars[j + k] {
                return Ordering::Less
            }

            if chars[i + k] > chars[j + k] {
                return Ordering::Greater
            }
        };

        Ordering::Equal
    });

    indices
}

fn main() {
    let text = read_line().trim().to_string();
    let result = calc_suffix_array(&text);
    println!("{}", result.iter().map(|k| k.to_string()).collect::<Vec<_>>().join(" "))
}
