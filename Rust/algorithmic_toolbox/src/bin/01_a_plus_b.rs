use std::io;

fn add_two_numbers(a: i32, b: i32) -> i32 {
    a + b
}

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn main() {
    let x = read_int();
    let y = read_int();
    let result = add_two_numbers(x, y);
    println!("{}", result);
}
