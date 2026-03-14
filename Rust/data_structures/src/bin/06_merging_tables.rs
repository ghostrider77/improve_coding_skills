use std::io;

struct TableOperation {
    source: usize,
    destination: usize,
}

struct Database {
    table_rows: Vec<i32>,
    parents: Vec<usize>,
    ranks: Vec<i32>,
}

impl Database {
    fn initialize(nr_tables: i32, table_rows: Vec<i32>) -> Self {
        let parents = (0..nr_tables as usize).collect();
        let ranks = vec![0; nr_tables as usize];
        Database {table_rows, parents, ranks}
    }

    fn find(&mut self, mut child_ix: usize) -> usize {
        let mut node_indices_on_path = Vec::new();
        loop {
            let parent_ix = self.parents[child_ix];
            if parent_ix == child_ix {
                node_indices_on_path.iter().for_each(|ix| self.parents[*ix] = parent_ix);
                return parent_ix
            }

            node_indices_on_path.push(child_ix);
            child_ix = parent_ix;
        }
    }

    fn union(&mut self, source: usize, destination: usize, largest_table_size: i32) -> i32 {
        let source_id = self.find(source);
        let dest_id = self.find(destination);
        if source_id == dest_id {
            largest_table_size

        } else if self.ranks[source_id] > self.ranks[dest_id] {
            self.parents[dest_id] = source_id;
            self.table_rows[source_id] += self.table_rows[dest_id];
            self.table_rows[dest_id] = 0;
            largest_table_size.max(self.table_rows[source_id])

        } else {
            self.parents[source_id] = dest_id;
            self.table_rows[dest_id] += self.table_rows[source_id];
            self.table_rows[source_id] = 0;
            if self.ranks[source_id] == self.ranks[dest_id] {
                self.ranks[dest_id] += 1;
            }

            largest_table_size.max(self.table_rows[dest_id])
        }
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

fn read_pair() -> (i32, i32) {
    let line = read_line();
    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_table_operations(nr_operations: i32) -> Vec<TableOperation> {
    let mut operations = Vec::with_capacity(nr_operations as usize);
    for _ in 0..nr_operations {
        let (d, s) = read_pair();
        operations.push(TableOperation {destination: (d - 1) as usize, source: (s - 1) as usize});
    }

    operations
}

fn process_merge_requests(table_rows: Vec<i32>, nr_tables: i32, operations: Vec<TableOperation>) -> Vec<i32> {
    let mut largest_table_size = *table_rows.iter().max().unwrap();
    let mut db = Database::initialize(nr_tables, table_rows);
    let mut largest_table_sizes = Vec::new();
    for TableOperation {source, destination} in operations {
        largest_table_size = db.union(source, destination, largest_table_size);
        largest_table_sizes.push(largest_table_size);
    }

    largest_table_sizes
}

fn main() {
    let (nr_tables, nr_operations) = read_pair();
    let table_rows = convert_to_int_vector(&read_line());
    let operations = read_table_operations(nr_operations);
    let result = process_merge_requests(table_rows, nr_tables, operations);
    result.iter().for_each(|s| println!("{}", s));
}
