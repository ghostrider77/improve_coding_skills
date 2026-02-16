use std::io;

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn fibonacci(n: i32) -> i32 {
    let mut k = 1;
    let mut a = 0;
    let mut b = 1;
    while k <= n {
        (a, b) = (b, a + b);
        k += 1;
    }
    a
}

fn main() {
    let n = read_int();
    let result = fibonacci(n);
    println!("{}", result);
}
