use std::{cmp::min, io};

#[derive(Debug)]
struct Item {
    value: i32,
    weight: i32,
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn parse_line() -> (i32, i32) {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_items(nr_items: usize) -> Vec<Item> {
    let mut items = Vec::with_capacity(nr_items);
    for _ in 0..nr_items {
        let (value, weight) = parse_line();
        items.push(Item {value, weight});

    }
    items
}

fn fractional_knapsack(mut items: Vec<Item>, mut capacity: i32) -> f64 {
    items.sort_by(|a, b| (b.value as f64 / b.weight as f64).partial_cmp(&(a.value as f64 / a.weight as f64)).unwrap());

    let mut total_value = 0.0;
    for item in &items {
        if capacity == 0 {
            return total_value
        }

        let amount = min(item.weight, capacity);
        total_value += amount as f64 * (item.value as f64 / item.weight as f64);
        capacity -= amount;
    }

    total_value
}

fn main() {
    let (nr_items, capacity) = parse_line();
    let items = read_items(nr_items as usize);
    let result = fractional_knapsack(items, capacity);
    println!("{}", result);
}
