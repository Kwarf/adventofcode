use std::fs::read_to_string;

use itertools::Itertools;

fn parse_line(line: &str) -> (u32, u32) {
    let mut iter = line.split_whitespace();
    let left = iter.next().unwrap().parse().unwrap();
    let right = iter.next().unwrap().parse().unwrap();
    (left, right)
}

fn main() {
    let pairs = read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(parse_line)
        .collect::<Vec<(u32, u32)>>();

    let lefts = pairs
        .iter()
        .map(|(left, _)| left)
        .sorted()
        .collect::<Vec<&u32>>();

    let rights = pairs
        .iter()
        .map(|(_, right)| right)
        .sorted()
        .collect::<Vec<&u32>>();

    println!(
        "The answer to the first part is: {}",
        lefts
            .iter()
            .zip(rights.iter())
            .map(|(&left, &right)| (*left as i32 - *right as i32).abs() as u32)
            .sum::<u32>()
    );
}
