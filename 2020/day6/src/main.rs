use std::{collections::HashSet, fs};

fn parse_group(group: &str) -> HashSet<char> {
    group.chars().filter(|&x| x != '\n').collect()
}

fn main() {
    println!(
        "The answer to the first part is: {}",
        fs::read_to_string("input.txt")
            .expect("Input file not found")
            .split("\n\n")
            .map(parse_group)
            .fold(0, |acc, x| acc + x.len())
    );
}
