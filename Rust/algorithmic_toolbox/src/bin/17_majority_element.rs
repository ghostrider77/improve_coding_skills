use std::collections::HashMap;
use std::io;

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn read_line() -> Vec<i32> {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.split_whitespace().map(|x| x.parse().unwrap()).collect()
}

fn has_majority_element(xs: Vec<i32>, n: i32) -> bool {
    let mut counts = HashMap::new();
    for x in xs {
        *counts.entry(x).or_insert(0) += 1;
    }

    counts.values().any(|&v| v > n / 2)
}

fn main() {
    let n = read_int();
    let xs = read_line();
    let result = has_majority_element(xs, n);
    println!("{}", if result {1} else {0});
}
