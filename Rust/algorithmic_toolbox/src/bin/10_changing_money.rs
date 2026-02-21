use std::io;

const COINS: [i32; 3] = [10, 5, 1];

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn calc_minimum_number_of_changes(mut amount: i32) -> i32 {
    let mut nr_changes = 0;
    for coin in COINS {
        nr_changes += amount / coin;
        amount = amount % coin;
    }
    nr_changes
}

fn main() {
    let amount = read_int();
    let result = calc_minimum_number_of_changes(amount);
    println!("{}", result);
}
