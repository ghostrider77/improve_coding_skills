use std::io;

const BRUTE_FORCE_SIZE: usize = 3;

#[derive(Debug, Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn distance(&self, other: &Point) -> f64 {
        (self.x - other.x).hypot(self.y - other.y)
    }
}

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

fn read_int() -> i32 {
    read_line().trim().parse().unwrap()
}

fn read_pair() -> (i32, i32) {
    let line = read_line();
    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_points(nr_points: usize) -> Vec<Point> {
    let mut points = Vec::with_capacity(nr_points);
    for _ in 0..nr_points {
        let (x, y) = read_pair();
        points.push(Point {x: x as f64, y: y as f64});
    }

    points
}

fn find_points_in_stripe(first: &[Point], second: &[Point], m: f64, delta: f64) -> Vec<Point> {
    let mut stripe = Vec::new();
    let mut add_points_to_stripe = |half_plane: &[Point]| {
        for p in half_plane.iter() {
            if (p.x - m).abs() <= delta {
                stripe.push(*p);
            }
        }
    };

    add_points_to_stripe(first);
    add_points_to_stripe(second);
    stripe
}

fn calc_minimum_distance_in_stripe(first: &[Point], second: &[Point], m: f64, delta: f64) -> f64 {
    let mut stripe = find_points_in_stripe(first, second, m, delta);
    stripe.sort_by(|p, q| p.y.partial_cmp(&q.y).unwrap());
    get_smallest_pairwise_distances(&stripe, delta, 7)
}

fn get_smallest_pairwise_distances(points: &[Point], mut min_dist: f64, compare_with: usize) -> f64 {
    for (ix, p) in points.iter().enumerate() {
        for q in points[(ix+1)..(ix+compare_with+1).min(points.len())].iter() {
            let dist = p.distance(q);
            if dist < min_dist {
                min_dist = dist;
            }
        }
    }
    min_dist
}

fn find_closest_points(sorted_points: &[Point], length: usize) -> f64 {
    if length <= BRUTE_FORCE_SIZE {
        return get_smallest_pairwise_distances(sorted_points, f64::INFINITY, BRUTE_FORCE_SIZE-1)
    }
    let middle = length / 2;
    let median_x = sorted_points[middle].x;
    let (first, second) = sorted_points.split_at(middle);
    let delta_1 = find_closest_points(first, middle);
    let delta_2 = find_closest_points(second, length - middle);
    let delta = delta_1.min(delta_2);
    if delta.abs() < 1e-14 {
        return 0.0
    }
    calc_minimum_distance_in_stripe(first, second, median_x, delta)
}

fn find_closest_pair_of_points(mut points: Vec<Point>, n: usize) -> f64 {
    points.sort_by(|p, q| p.x.partial_cmp(&q.x).unwrap());
    find_closest_points(&points, n)
}

fn main() {
    let n = read_int();
    let points = read_points(n as usize);
    let result = find_closest_pair_of_points(points, n as usize);
    println!("{}", result);
}
