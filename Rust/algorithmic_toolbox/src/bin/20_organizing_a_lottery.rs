use std::io;

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

fn read_segments(n: usize) -> (Vec<i32>, Vec<i32>) {
    let mut segments = Vec::with_capacity(n);
    for _ in 0..n {
        let endpoints = read_pair();
        segments.push(endpoints);
    }

    segments.into_iter().unzip()
}

fn get_number_of_suitable_endpoints(endpoints: &[i32], point: i32) -> usize {
    fn binary_search(endpoints: &[i32], point: i32, a: usize, b: usize) -> usize {
        if a == b {
            a
        } else {
            let mid = (a + b) / 2;
            if endpoints[mid] <= point {
                binary_search(endpoints, point, mid + 1, b)
            } else {
                binary_search(endpoints, point, a, mid)
            }
        }
    }

    let n = endpoints.len();
    if endpoints[n - 1] <= point {
        n
    } else {
        binary_search(endpoints, point, 0, n - 1)
    }
}

fn calc_intersection_size(sorted_left: &[i32], sorted_negated_right: &[i32], nr_segments: i32, point: i32) -> i32 {
    let nr_good_left_ends = get_number_of_suitable_endpoints(sorted_left, point);
    let nr_good_right_ends = get_number_of_suitable_endpoints(sorted_negated_right, -point);
    (nr_good_left_ends + nr_good_right_ends) as i32 - nr_segments
}

fn number_of_segments_containing_points(
    left_endpoints: Vec<i32>,
    right_endpoints: Vec<i32>,
    nr_segments: i32,
    points: Vec<i32>,
) -> Vec<i32> {
    let mut sorted_left_endpoints = left_endpoints.clone();
    sorted_left_endpoints.sort();
    let mut sorted_negated_right_endpoints = right_endpoints.iter().map(|b| -b).collect::<Vec<_>>();
    sorted_negated_right_endpoints.sort();
    points
        .iter()
        .map(|p| calc_intersection_size(&sorted_left_endpoints, &sorted_negated_right_endpoints, nr_segments, *p))
        .collect()
}

fn main() {
    let (nr_segments, _) = read_pair();
    let (left_endpoints, right_endpoints) = read_segments(nr_segments as usize);
    let points = convert_to_int_vector(&read_line());
    let result = number_of_segments_containing_points(left_endpoints, right_endpoints, nr_segments, points);
    println!("{}", result.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}
