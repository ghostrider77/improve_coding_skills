use std::io;

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn parse_input() -> (i32, i32) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn calc_gcd(mut a: i32, mut b: i32) -> i32 {
    while b > 0 {
        (a, b) = (b, a % b);
    }
    a
}

fn calc_lcm(a: i32, b: i32) -> i64 {
    let gcd = calc_gcd(a, b);
    ((a / gcd) as i64) * (b as i64)
}

fn main() {
    let (a, b) = parse_input();
    let result = calc_lcm(a, b);
    println!("{}", result);
}
