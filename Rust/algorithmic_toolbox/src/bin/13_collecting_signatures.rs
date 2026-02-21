use std::io;

#[derive(Debug)]
struct Segment {
    left: i32,
    right: i32,
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn parse_line() -> (i32, i32) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn read_segments(n: usize) -> Vec<Segment> {
    let mut segments = Vec::with_capacity(n);
    for _ in 0..n {
        let (left, right) = parse_line();
        segments.push(Segment {left, right});

    }
    segments
}

fn calc_minimum_number_of_points_covering_segments(mut segments: Vec<Segment>) -> Vec<i32> {
    segments.sort_by_key(|s| s.right);

    let mut points = Vec::new();
    while let Some(first) = segments.first() {
        let b = first.right;
        points.push(b);
        segments.retain(|s| s.left > b);
    }

    points
}

fn main() {
    let nr_segments = read_int();
    let segments = read_segments(nr_segments as usize);
    let covering = calc_minimum_number_of_points_covering_segments(segments);
    println!("{}", covering.len());
    println!("{}", covering.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "))
}
