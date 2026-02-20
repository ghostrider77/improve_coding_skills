use std::io;

fn convert_to_int_vector(line: &str) -> Vec<i64> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn parse_input() -> (i64, i64) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let xs: Vec<i64> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn calc_pisano_period(modulus: i64) -> i64 {
    let mut p = 1;
    let mut a = 1;
    let mut b = 1;
    while !(a == 0 && b == 1) {
        (a, b) = (b, (a + b) % modulus);
        p += 1;
    }
    p
}

fn calc_fibonacci_modulo(n: i64, modulus: i64) -> i64 {
    let mut k = 1;
    let mut a = 0;
    let mut b = 1;
    while k <= n {
        (a, b) = (b, (a + b) % modulus);
        k += 1;
    }
    a
}

fn calc_last_digit_of_partial_sum(m: i64, n: i64, modulus: i64) -> i64 {
    let p = calc_pisano_period(modulus);
    let last_digit_of_prefix_sum = (calc_fibonacci_modulo((m + 1) % p, modulus) - 1).rem_euclid(modulus);
    let last_digit_of_full_sum = (calc_fibonacci_modulo((n + 2) % p, modulus) - 1).rem_euclid(modulus);
    (last_digit_of_full_sum - last_digit_of_prefix_sum).rem_euclid(modulus)
}

fn main() {
    let (m, n) = parse_input();
    let modulus = 10;
    let result = calc_last_digit_of_partial_sum(m, n, modulus);
    println!("{}", result);
}
