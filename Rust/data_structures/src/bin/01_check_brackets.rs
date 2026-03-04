use std::io;

static OPENING_BRACKETS: [char; 3] = ['(', '[', '{'];
static CLOSING_BRACKETS: [char; 3] = [')', ']', '}'];

struct OpenedBracket {
    bracket: char,
    position: usize,
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.trim().to_string()
}

fn do_brackets_match(opening: char, closing: char) -> bool {
    (opening == '(' && closing == ')') || (opening == '[' && closing == ']') || (opening == '{' && closing == '}')
}

fn find_index_of_non_matching_bracket(string: &str) -> Option<usize> {
    let mut stack = Vec::new();
    for (ix, letter) in string.chars().enumerate() {
        if OPENING_BRACKETS.contains(&letter) {
            stack.push(OpenedBracket {bracket: letter, position: ix});

        } else if CLOSING_BRACKETS.contains(&letter) {
            let Some(OpenedBracket {bracket, ..}) = stack.pop() else {
                return Some(ix)
            };
            if !do_brackets_match(bracket, letter) {
                return Some(ix)
            }

        }
    }

    stack.pop().map(|OpenedBracket {position, ..}| position)
}

fn main(){
    let string = read_line();
    let result = find_index_of_non_matching_bracket(&string);
    match result {
        None => println!("Success"),
        Some(ix) => println!("{}", ix + 1),
    }
}
