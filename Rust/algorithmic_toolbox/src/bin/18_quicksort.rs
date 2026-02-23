use std::io;
use rand::{RngExt, SeedableRng};
use rand::rngs::StdRng;

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn read_vector() -> Vec<i32> {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.split_whitespace().map(|x| x.parse().unwrap()).collect()
}

fn three_way_partitioning(xs: &mut [i32], pivot: i32, mut start: usize, mut end: usize) -> (usize, usize) {
    let mut ix = start;
    while ix <= end {
        let elem = xs[ix];
        if elem < pivot {
            if ix != start {
                (xs[ix], xs[start]) = (xs[start], elem);
            }
            ix += 1;
            start += 1;

        } else if elem > pivot {
            (xs[ix], xs[end]) = (xs[end], elem);
            end -= 1;

        } else {
            ix += 1;
        }
    }

    (start, end)
}

fn quicksort(xs: &mut [i32], n: usize) -> () {
    let mut rng = StdRng::seed_from_u64(2112);
    let mut stack: Vec<(usize, usize)> = Vec::new();
    stack.push((0, n - 1));
    loop {
        let Some((left_end, right_end)) = stack.pop() else {
            return;
        };
        if left_end < right_end {
            let ix = rng.random_range(left_end..=right_end);
            let pivot = xs[ix];
            let (middle_start, middle_end) = three_way_partitioning(xs, pivot, left_end, right_end);
            if middle_start > 0 {
                stack.push((left_end, middle_start-1));
            }
            stack.push((middle_end+1, right_end));
        }
    }
}

fn main() {
    let n = read_int();
    let mut xs = read_vector();
    quicksort(&mut xs, n as usize);
    println!("{}", xs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}
