use std::io;

const MAX_SIZE: i32 = 10000000;

struct PhoneBook {
    phone_book: Vec<Option<String>>
}

impl PhoneBook {
    fn new(max_size: usize) -> Self {
        PhoneBook {phone_book: vec![None; max_size]}
    }

    fn add(&mut self, number: usize, name: String) {
        self.phone_book[number] = Some(name);
    }

    fn delete(&mut self, number: usize) {
        self.phone_book[number] = None;
    }

    fn find(&self, number: usize) -> Option<String> {
        self.phone_book[number].clone()
    }
}

enum Query {
    Addition {number: i32, name: String},
    Deletion {number: i32},
    Find {number: i32},
}

impl Query {
    fn from_string(s: &str) -> Self {
        match s.split_whitespace().collect::<Vec<_>>().as_slice() {
            ["add", number, name] => Query::Addition {number: number.parse().unwrap(), name: name.to_string()},
            ["del", number] => Query::Deletion {number: number.parse().unwrap()},
            ["find", number] => Query::Find {number: number.parse().unwrap()},
            _ => panic!("Unknown query type: {}", s),
        }
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

fn process_queries(queries: Vec<Query>) -> Vec<String> {
    let mut phone_book = PhoneBook::new(MAX_SIZE as usize);
    let mut result = Vec::new();
    for query in queries {
        match query {
            Query::Addition {number, name} => phone_book.add(number as usize, name),
            Query::Deletion {number} => phone_book.delete(number as usize),
            Query::Find {number} => {
                let res = phone_book.find(number as usize);
                result.push(res.map(|r| r.to_string()).unwrap_or_else(|| String::from("not found")));
            }

        }
    }

    result
}

fn main() {
    let nr_queries = read_line().trim().parse().unwrap();
    let queries = read_queries(nr_queries);
    let result = process_queries(queries);
    result.iter().for_each(|r| println!("{}", r));
}
