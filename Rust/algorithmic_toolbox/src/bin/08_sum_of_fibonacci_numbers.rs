use std::io;

fn read_long() -> i64 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
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

fn calc_last_digit_of_the_sum_of_fibonacci_numbers(n: i64, modulus: i64) -> i64 {
    let p = calc_pisano_period(modulus);
    (calc_fibonacci_modulo((n + 2) % p, modulus) - 1).rem_euclid(modulus)
}

fn main() {
    let n = read_long();
    let modulus = 10;
    let result = calc_last_digit_of_the_sum_of_fibonacci_numbers(n, modulus);
    println!("{}", result);
}
