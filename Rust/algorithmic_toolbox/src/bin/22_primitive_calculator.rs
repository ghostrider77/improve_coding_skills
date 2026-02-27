use std::io;

const NOMINATORS: [usize; 2] = [2, 3];

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn find_previous_minimum(operations: &[i32], k: usize) -> (i32, usize) {
    let mut m = k - 2;
    let mut previous_minimum = operations[m];
    let mut min_arg = m;
    for nominator in NOMINATORS {
        if k % nominator == 0 {
            m = k / nominator - 1;
            let nr_ops = operations[m];
            if nr_ops < previous_minimum {
                previous_minimum = nr_ops;
                min_arg = m;
            }
        }
    }

    (previous_minimum, min_arg)
}

fn backtrack_calculation(backtrack: &[usize], n: usize) -> Vec<usize> {
    let mut path = vec![n];
    let mut k = n - 1;
    while k > 1 {
        k = backtrack[k];
        path.push(k + 1);
    }

    path.reverse();
    path
}

fn run_calculator(n: usize) -> Vec<usize> {
    let mut min_operations = vec![0; n];
    let mut backtrack = vec![0; n];
    for k in 2..=n {
        let (previous_minimum, arg) = find_previous_minimum(&min_operations, k);
        min_operations[k-1] = previous_minimum + 1;
        backtrack[k-1] = arg;
    }
    backtrack_calculation(&backtrack, n)
}

fn main() {
    let n = read_int();
    let result = run_calculator(n as usize);
    println!("{}", result.len() - 1);
    println!("{}", result.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}
