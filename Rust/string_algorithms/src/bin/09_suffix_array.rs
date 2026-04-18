use std::io;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn calc_suffix_array(text: &str) -> Vec<usize> {
    let bytes = text.as_bytes();
    let mut indices: Vec<usize> = (0..bytes.len()).collect();

    indices.sort_by(|&i, &j| bytes[i..].cmp(&bytes[j..]));
    indices
}

fn main() {
    let text = read_line().trim().to_string();
    let result = calc_suffix_array(&text);
    println!("{}", result.iter().map(|k| k.to_string()).collect::<Vec<_>>().join(" "));
}
