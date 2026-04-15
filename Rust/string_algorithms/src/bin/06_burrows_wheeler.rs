use std::cmp::Ordering;
use std::io;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn burrows_wheeler_transform(s: String) -> String {
    let n = s.len();
    let ds = s.repeat(2).chars().collect::<Vec<_>>();
    let mut indices: Vec<usize> = (0..n).collect();
    indices.sort_by(|i, j| {
        for k in 0..n {
            if ds[i + k] < ds[j + k] {
                return Ordering::Less
            }

            if ds[i + k] > ds[j + k] {
                return Ordering::Greater
            }
        };

        Ordering::Equal
    });
    indices.iter().map(|&ix| ds[ix + n - 1]).collect()
}

fn main() {
    let text = read_line().trim().to_string();
    let result = burrows_wheeler_transform(text);
    println!("{}", result);
}
