use std::io;

fn read_int() -> i32 {
    let mut x = String::new();
    io::stdin().read_line(&mut x).unwrap();
    x.trim().parse().unwrap()
}

fn get_decomposition_to_maximal_number_of_distinct_elems(mut n: i32) -> Vec<i32> {
    let mut summands = Vec::new();
    let mut smallest_summand = 1;
    while n > 0 {
        let elem = if n > 2 * smallest_summand { smallest_summand } else { n };
        summands.push(elem);
        n -= elem;
        smallest_summand += 1;
    }

    summands
}

fn main() {
    let n = read_int();
    let result = get_decomposition_to_maximal_number_of_distinct_elems(n);
    println!("{}", result.len());
    println!("{}", result.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "))
}
