use std::fs::read_to_string;

use itertools::Itertools;
use regex::Regex;

fn process(s: &str) -> u64 {
    Regex::new(r"mul\((\d+),(\d+)\)")
        .unwrap()
        .captures_iter(s)
        .map(|c| {
            let (_, [a, b]) = c.extract();
            a.parse::<u64>().unwrap() * b.parse::<u64>().unwrap()
        })
        .sum()
}

fn main() {
    let input = read_to_string("input.txt").unwrap();

    println!("The answer to the first part is: {}", process(&input));

    let controls = input
        .match_indices("don't()")
        .map(|(i, _)| (i, false))
        .chain(input.match_indices("do()").map(|(i, _)| (i, true)))
        .sorted_by_key(|(i, _)| *i)
        .collect::<Vec<(usize, bool)>>();

    println!(
        "The answer to the second part is: {}",
        controls
            .iter()
            .tuple_windows()
            .filter(|(a, _)| a.1)
            .map(|(a, b)| process(&input[a.0..b.0]))
            .sum::<u64>()
            + process(&input[..controls[0].0])
            + if controls.last().unwrap().1 {
                process(&input[controls.last().unwrap().0..])
            } else {
                0
            }
    );
}
