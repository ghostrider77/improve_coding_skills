use std::{cmp::max, io};

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line

}

fn read_pair() -> (i32, i32) {
    let line = read_line();
    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn solve_knapsack_problem(weights: &[i32], nr_weights: i32, capacity: i32) -> i32 {
    fn solve(current_capacity: usize, n: usize, knapsack: &mut Vec<Vec<i32>>, weights: &[i32]) -> i32 {
        if current_capacity == 0 || n == 0 {
            return 0
        }

        if knapsack[current_capacity-1][n-1] != -1 {
            return knapsack[current_capacity-1][n-1]
        }

        let weight = weights[n-1];
        let optimal_weight = if current_capacity < weight as usize {
            solve(current_capacity, n - 1, knapsack, weights)
        } else {
            max(
                solve(current_capacity - weight as usize, n - 1, knapsack, weights) + weight,
                solve(current_capacity, n - 1, knapsack, weights)
            )
        };
        knapsack[current_capacity-1][n-1] = optimal_weight;
        optimal_weight
    }

    let mut knapsack = vec![vec![-1; nr_weights as usize]; capacity as usize];
    solve(capacity as usize, nr_weights as usize, &mut knapsack, weights)
}

fn main() {
    let (capacity, nr_weights) = read_pair();
    let weights = convert_to_int_vector(&read_line());
    let result = solve_knapsack_problem(&weights, nr_weights, capacity);
    println!("{}", result);
}
