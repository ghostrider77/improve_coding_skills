use std::io;

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.trim().to_string()
}

fn calc_maximal_revenue(mut profit_per_click: Vec<i32>, mut average_click: Vec<i32>) -> i64 {
    profit_per_click.sort();
    average_click.sort();

    profit_per_click
        .iter()
        .zip(average_click.iter())
        .fold(0, |acc, (p, a)| acc + (*p as i64) * (*a as i64))
}

fn main() {
    let _ = read_int();
    let profit_per_click = convert_to_int_vector(&read_line());
    let average_click = convert_to_int_vector(&read_line());
    let result = calc_maximal_revenue(profit_per_click, average_click);
    println!("{}", result);
}
