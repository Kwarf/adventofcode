use std::fs::read_to_string;

use regex::Regex;

fn main() {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    let input = read_to_string("input.txt").unwrap();

    println!(
        "The answer to the first part is: {}",
        re.captures_iter(&input)
            .map(|c| {
                let (_, [a, b]) = c.extract();
                a.parse().unwrap_or(0) * b.parse().unwrap_or(0)
            })
            .sum::<i64>()
    );
}
