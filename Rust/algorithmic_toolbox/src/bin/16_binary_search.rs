use std::io;

fn read_vector() -> Vec<i32> {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .skip(1)
        .collect()
}

fn find_elems(xs: Vec<i32>, queries: Vec<i32>) -> Vec<i32> {
    queries
        .iter()
        .map(|q| xs.binary_search(q).map_or(-1, |ix| ix as i32))
        .collect()
}

fn main() {
    let xs = read_vector();
    let queries = read_vector();
    let result = find_elems(xs, queries);
    println!("{}", result.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}
