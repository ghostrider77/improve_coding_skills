use std::io;

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_sequence() -> Vec<i32> {
    fn read_line() -> String {
        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();
        line
    }

    let _ = read_line();
    convert_to_int_vector(&read_line())
}

fn calc_longest_common_subsequence(s1: &[i32], s2: &[i32], s3: &[i32]) -> i32 {
    let n1 = s1.len();
    let n2 = s2.len();
    let n3 = s3.len();
    let mut longest_path = vec![vec![vec![0; n3 + 1]; n2 + 1]; n1 + 1];

    for i in 0..n1 {
        for j in 0..n2 {
            for k in 0..n3 {
                if s1[i] == s2[j] && s1[i] == s3[k] {
                    longest_path[i+1][j+1][k+1] = longest_path[i][j][k] + 1;
                } else {
                    let a = longest_path[i][j+1][k+1];
                    let b = longest_path[i+1][j][k+1];
                    let c = longest_path[i+1][j+1][k];
                    longest_path[i+1][j+1][k+1] = a.max(b).max(c);
                }
            }
        }
    }

    longest_path[n1][n2][n3]
}

fn main() {
    let s1 = read_sequence();
    let s2 = read_sequence();
    let s3 = read_sequence();
    let result = calc_longest_common_subsequence(&s1, &s2, &s3);
    println!("{}", result);
}
