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

fn get_index_of_parent_children_minimum(array: &[i32], parent_index: usize, size: usize) -> usize {
    let mut min_index = parent_index;
    let left_child_ix = 2*parent_index + 1;
    if left_child_ix < size && array[left_child_ix] < array[min_index] {
        min_index = left_child_ix;
    }
    let right_child_ix = left_child_ix + 1;
    if right_child_ix < size && array[right_child_ix] < array[min_index] {
        min_index = right_child_ix;
    }
    min_index
}

fn sift_down(array: &mut [i32], mut parent_index: usize, size: usize, swaps: &mut Vec<(usize, usize)>) {
    let mut min_index = get_index_of_parent_children_minimum(array, parent_index, size);
    while min_index != parent_index {
        array.swap(min_index, parent_index);
        swaps.push((parent_index, min_index));
        parent_index = min_index;
        min_index = get_index_of_parent_children_minimum(array, parent_index, size);
    }
}

fn heapify(array: &mut [i32], n: usize) -> Vec<(usize, usize)> {
    let mut swaps = Vec::new();
    let mut parent_ix = n / 2;
    loop {
        if parent_ix <= 0 {
            return swaps
        }
        parent_ix -= 1;
        sift_down(array, parent_ix, n, &mut swaps);
    }
}

fn main() {
    let n = read_line().trim().parse().unwrap();
    let mut array = convert_to_int_vector(&read_line());
    let result = heapify(&mut array, n);
    println!("{}", result.len());
    result.iter().for_each(|(i, j)| println!("{i} {j}"));
}
