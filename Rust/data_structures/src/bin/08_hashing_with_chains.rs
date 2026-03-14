use std::io;

enum Query {
    Addition(String),
    Deletion(String),
    Find(String),
    Check(i32),
}

impl Query {
    fn from_string(s: &str) -> Self {
        match s.split_whitespace().collect::<Vec<_>>().as_slice() {
            ["add", str] => Query::Addition(str.to_string()),
            ["del", str] => Query::Deletion(str.to_string()),
            ["find", str] => Query::Find(str.to_string()),
            ["check", bucket_id] => Query::Check(bucket_id.parse().unwrap()),
            _ => panic!("Unknown query type: {}", s),
        }
    }
}

struct HashTable {
    prime: i64,
    x: i64,
    size: i64,
    table: Vec<Vec<String>>,
}

impl HashTable {
    fn new(prime: i64, x: i64, cardinality: i64) -> Self {
        let table = vec![Vec::new(); cardinality as usize];
        HashTable { prime, x, size: cardinality, table }
    }

    fn add(&mut self, string: String) {
        let hash_value = self.polynomial_hashing(&string) as usize;
        let chain = &mut self.table[hash_value];
        if !chain.contains(&string) {
            chain.push(string);
        }
    }

    fn delete(&mut self, string: String) {
        let hash_value = self.polynomial_hashing(&string) as usize;
        let chain = &mut self.table[hash_value];
        chain.retain(|s| s != &string)
    }

    fn find(&mut self, string: String) -> String {
        let hash_value = self.polynomial_hashing(&string) as usize;
        let chain = &mut self.table[hash_value];
        if chain.contains(&string) { String::from("yes") } else { String::from("no") }
    }

    fn check(&self, bucket_id: i32) -> String {
        let chain = &self.table[bucket_id as usize];
        chain.iter().rev().cloned().collect::<Vec<_>>().join(" ")
    }

    fn polynomial_hashing(&self, s: &str) -> i64 {
        let mut value = 0;
        for code in s.bytes().rev() {
            value = (value * self.x + code as i64) % self.prime
        }

        value % self.size
    }
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line
}

fn read_queries(nr_queries: usize) -> Vec<Query> {
    let mut queries = Vec::with_capacity(nr_queries);
    for _ in 0..nr_queries {
        let line = read_line();
        let query = Query::from_string(&line);
        queries.push(query);
    }

    queries
}

fn process_queries(queries: Vec<Query>, prime: i64, x: i64, cardinality: i64) -> Vec<String> {
    let mut hash_table = HashTable::new(prime, x, cardinality);
    let mut result = Vec::new();
    for query in queries {
        match query {
            Query::Addition(string) => hash_table.add(string),
            Query::Deletion(string) => hash_table.delete(string),
            Query::Find(string) => result.push(hash_table.find(string)),
            Query::Check(bucket) => result.push(hash_table.check(bucket)),
        }
    }

    result
}

fn main() {
    let cardinality = read_line().trim().parse().unwrap();
    let nr_queries = read_line().trim().parse().unwrap();
    let prime = 1000000007;
    let x = 263;
    let queries = read_queries(nr_queries);
    let result = process_queries(queries, prime, x, cardinality);
    result.iter().for_each(|r| println!("{}", r));
}
