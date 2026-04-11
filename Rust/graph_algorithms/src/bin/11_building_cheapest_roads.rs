use std::collections::HashMap;
use std::f64::INFINITY;
use std::io;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn distance(&self, other: &Self) -> f64 {
        ((self.x - other.x) as f64).hypot((self.y - other.y) as f64)
    }
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line
        .split_whitespace()
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

fn read_points(nr_points: i32) -> Vec<Point> {
    let mut points = Vec::with_capacity(nr_points as usize);
    for _ in 0..nr_points {
        let (x, y) = read_pair();
        points.push(Point { x, y })
    }

    points
}

fn calc_minimal_spanning_tree(points: Vec<Point>) -> f64 {
    let mut total_cost = 0.0;
    let Some(start_point) = points.iter().next() else {
        return total_cost
    };
    let mut nodes_with_cost: HashMap<Point, f64> = HashMap::from_iter(points.iter().cloned().map(|p| (p, INFINITY)));
    nodes_with_cost.insert(start_point.clone(), 0.0);
    loop {
        let Some(v) =
                nodes_with_cost.iter().min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap()).map(|(k, _)| k.clone()) else {
            return total_cost
        };
        let (_, cost_of_adding_v) = nodes_with_cost.remove_entry(&v).unwrap();
        total_cost += cost_of_adding_v;
        for (z, cost_of_adding_z) in nodes_with_cost.iter_mut() {
            let dist = v.distance(z);
            if *cost_of_adding_z > dist {
                *cost_of_adding_z = dist;
            }
        }
    }
}

fn main() {
    let nr_points = read_line().trim().parse().unwrap();
    let points = read_points(nr_points);
    let result = calc_minimal_spanning_tree(points);
    println!("{}", result);
}
