use phf::phf_map;
use std::{i64, io};

static OPS: phf::Map<&'static str, fn(i64, i64) -> i64> = phf_map! {
    "+" => |a, b| a + b,
    "-" => |a, b| a - b,
    "*" => |a, b| a * b,
};

fn read_input_data() -> (Vec<i64>, Vec<String>) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let mut digits = Vec::new();
    let mut operations = Vec::new();
    for (ix, chr) in line.trim().chars().enumerate() {
        if ix % 2 == 0 {
            digits.push(chr.to_digit(10).unwrap() as i64);
        } else {
            operations.push(chr.to_string());
        }
    }

    (digits, operations)
}

fn calc_min_max(
    i: usize,
    j: usize,
    operations: &[String],
    minimum_of_subexpressions: &Vec<Vec<i64>>,
    maximum_of_subexpressions: &Vec<Vec<i64>>,
) -> (i64, i64) {
    let mut subexpression_min = i64::MAX;
    let mut subexpression_max = i64::MIN;
    for k in i..j {
        let op = OPS[&operations[k]];
        let a = op(maximum_of_subexpressions[i][k], maximum_of_subexpressions[k+1][j]);
        let b = op(maximum_of_subexpressions[i][k], minimum_of_subexpressions[k+1][j]);
        let c = op(minimum_of_subexpressions[i][k], maximum_of_subexpressions[k+1][j]);
        let d = op(minimum_of_subexpressions[i][k], minimum_of_subexpressions[k+1][j]);
        subexpression_min = subexpression_min.min(a).min(b).min(c).min(d);
        subexpression_max = subexpression_max.max(a).max(b).max(c).max(d);
    }

    (subexpression_min, subexpression_max)
}

fn maximize_an_arithmetic_expression(digits: &[i64], operations: &[String]) -> i64 {
    let n = digits.len();
    let mut minimum_of_subexpressions = vec![vec![0; n]; n];
    let mut maximum_of_subexpressions = vec![vec![0; n]; n];
    for (ix, digit) in digits.iter().enumerate() {
        minimum_of_subexpressions[ix][ix] = *digit;
        maximum_of_subexpressions[ix][ix] = *digit;
    }

    for s in 1..n {
        for ix in 0..(n-s) {
            let jy = ix + s;
            let (min, max) = calc_min_max(ix, jy, operations, &minimum_of_subexpressions, &maximum_of_subexpressions);
            minimum_of_subexpressions[ix][jy] = min;
            maximum_of_subexpressions[ix][jy] = max;
        }
    }

    maximum_of_subexpressions[0][n-1]
}

fn main() {
    let (digits, operations) = read_input_data();
    let result = maximize_an_arithmetic_expression(&digits, &operations);
    println!("{}", result);
}
