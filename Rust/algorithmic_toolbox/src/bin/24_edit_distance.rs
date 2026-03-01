use std::io;

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line

}

fn calc_edit_distance(s1: &str, s2: &str) -> i32 {
    let s1: Vec<char> = s1.trim().chars().collect();
    let s2: Vec<char> = s2.trim().chars().collect();
    let n = s1.len();
    let m = s2.len();
    let mut edit_distance = vec![vec![0; m + 1]; n + 1];
    for ix in 1..=n {
        edit_distance[ix][0] = ix as i32;
    }

    for jy in 1..=m {
        edit_distance[0][jy] = jy as i32;
    }

    for ix in 0..n {
        for jy in 0..m {
            let deletion = edit_distance[ix][jy+1] + 1;
            let insertion = edit_distance[ix+1][jy] + 1;
            let mut matching = edit_distance[ix][jy];
            if s1[ix] != s2[jy] {
                matching += 1;
            }

            edit_distance[ix+1][jy+1] = insertion.min(deletion).min(matching);
        }
    }

    edit_distance[n][m]
}

fn main() {
    let s1 = read_line();
    let s2 = read_line();
    let result = calc_edit_distance(&s1, &s2);
    println!("{}", result);
}
