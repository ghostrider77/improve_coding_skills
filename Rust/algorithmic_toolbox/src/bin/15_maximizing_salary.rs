use std::io;

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn read_line() -> Vec<String> {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.split_whitespace().map(|x| x.to_string()).collect()
}

fn find_largest_number_from_pieces(mut numbers: Vec<String>) -> String {
    numbers.sort_by(|n, m| {
        let nm = format!("{n}{m}");
        let mn = format!("{m}{n}");
        mn.cmp(&nm)
    });
    numbers.concat()
}

fn main() {
    let _ = read_int();
    let numbers = read_line();
    let result = find_largest_number_from_pieces(numbers);
    println!("{}", result);
}
