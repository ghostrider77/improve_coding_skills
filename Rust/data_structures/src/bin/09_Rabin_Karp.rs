use std::io;
use rand::{RngExt, SeedableRng};
use rand::rngs::StdRng;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.trim().to_string()
}

fn polynomial_hashing(string: &str, prime: i64, x: i64) -> i64 {
    string
        .bytes()
        .rev()
        .fold(0, |acc, code| (acc * x + code as i64) % prime)
}

fn calc_power_of_x(x: i64, exponent: usize, prime: i64) -> i64 {
    (0..exponent).fold(1, |acc, _| acc * x % prime)
}

fn calc_substring_hashes(text: &str, text_length: usize, pattern_length: usize, prime: i64, x: i64) -> Vec<i64> {
    let length = text_length - pattern_length + 1;
    let mut hash_values = vec![0; length];
    let last_substring_in_text = &text[(text_length-pattern_length)..text_length];
    hash_values[length - 1] = polynomial_hashing(last_substring_in_text, prime, x);
    let x_power = calc_power_of_x(x, pattern_length, prime);
    let ascii = text.as_bytes();
    for ix in (0..(length - 1)).rev() {
        let value = x * hash_values[ix+1] + ascii[ix] as i64 - x_power * ascii[ix + pattern_length] as i64;
        hash_values[ix] = value.rem_euclid(prime);
    };

    hash_values
}

fn rabin_karp_algorithm(text: &str, pattern: &str, prime: i64) -> Vec<usize> {
    let mut rng = StdRng::seed_from_u64(2112);
    let x = rng.random_range(1..prime);
    let pattern_length = pattern.len();
    let text_length = text.len();
    let pattern_hash = polynomial_hashing(pattern, prime, x);
    let substring_hashes = calc_substring_hashes(text, text_length, pattern_length, prime, x);
    let mut matching_indices = Vec::new();
    for ix in 0..(text_length-pattern_length+1) {
        if pattern_hash == substring_hashes[ix] && &text[ix..(ix+pattern_length)] == pattern {
            matching_indices.push(ix);
        }
    }

    matching_indices
}

fn main() {
    let pattern = read_line();
    let text = read_line();
    let prime: i64 = 1000000007;
    let result = rabin_karp_algorithm(&text, &pattern, prime);
    println!("{}", result.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}
