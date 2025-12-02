use std::ops::Range;

use fancy_regex::Regex;
use std::sync::OnceLock;

static REGEX: OnceLock<Regex> = OnceLock::new();

fn is_invalid(num: usize) -> bool {
    let regex = REGEX.get_or_init(|| Regex::new(r"^(.+)\1$").unwrap());
    regex.is_match(&num.to_string()).unwrap()
}

fn parse_range(range_str: &str) -> Range<usize> {
    let parts: Vec<&str> = range_str.split('-').collect();
    Range {
        start: parts[0].parse().unwrap(),
        end: parts[1].parse().unwrap(),
    }
}

fn main() {
    let ranges = std::fs::read_to_string("input.txt")
        .unwrap()
        .split(',')
        .map(parse_range)
        .collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        ranges
            .iter()
            .flat_map(|x| x.start..=x.end)
            .filter(|x| is_invalid(*x))
            .sum::<usize>()
    );
}
