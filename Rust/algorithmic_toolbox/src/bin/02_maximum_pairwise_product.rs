use std::io;


fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.trim().to_string()
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect()
}

fn calc_maximum_pairwise_product(xs: &[i32]) -> i64 {
    let mut a = i32::MIN;
    let mut b = i32::MIN;
    for &x in xs {
        if x > a {
            b = a;
            a = x;
        } else if x > b {
            b = x;
        }
    }
    (a as i64) * (b as i64)
}

fn main() {
    let line = read_line();
    let xs = convert_to_int_vector(&line);
    let result = calc_maximum_pairwise_product(&xs);
    println!("{}", result);
}
