use std::io;

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

fn merge_sorted_arrays(xs: &[i32], ys: &[i32], length_xs: usize, length_ys: usize) -> (Vec<i32>, i64) {
    let mut merged = Vec::with_capacity(length_xs + length_ys);
    let mut ix = 0;
    let mut jy = 0;
    let mut inversions = 0;
    while (ix < length_xs) || (jy < length_ys) {
        if (ix == length_xs) && (jy < length_ys) {
            merged.push(ys[jy]);
            jy += 1;

        } else if (ix < length_xs) && (jy == length_ys) {
            merged.push(xs[ix]);
            ix += 1;

        } else {
            let x = xs[ix];
            let y = ys[jy];
            if x <= y {
                merged.push(x);
                ix += 1;
            } else {
                merged.push(y);
                jy += 1;
                inversions += (length_xs - ix) as i64;
            }
        }
    }

    (merged, inversions)
}

fn count_inversions(xs: &[i32], length: usize) -> (Vec<i32>, i64) {
    if length <= 1 {
        (xs.to_vec(), 0)
    } else {
        let middle = length / 2;
        let (first, second) = xs.split_at(middle);
        let (length1, length2) = (middle, length - middle);
        let (sorted_first, inversions_first) = count_inversions(first, length1);
        let (sorted_second, inversions_second) = count_inversions(second, length2);
        let (merged, inversions_split) = merge_sorted_arrays(&sorted_first, &sorted_second, length1, length2);
        (merged, inversions_first + inversions_second + inversions_split)
    }
}

fn main() {
    let n = read_int();
    let xs = read_vector();
    let (_, result) = count_inversions(&xs, n as usize);
    println!("{}", result);
}
