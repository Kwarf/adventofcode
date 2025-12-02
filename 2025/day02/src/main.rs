use std::ops::Range;

use fancy_regex::Regex;
use std::sync::OnceLock;

static P1_REGEX: OnceLock<Regex> = OnceLock::new();
static P2_REGEX: OnceLock<Regex> = OnceLock::new();

fn is_invalid(is_part_one: bool, num: usize) -> bool {
    let regex = if is_part_one {
        P1_REGEX.get_or_init(|| Regex::new(r"^(.+)\1$").unwrap())
    } else {
        P2_REGEX.get_or_init(|| Regex::new(r"^(.+)\1+$").unwrap())
    };
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
            .filter(|x| is_invalid(true, *x))
            .sum::<usize>()
    );

    println!(
        "The answer to the second part is: {}",
        ranges
            .iter()
            .flat_map(|x| x.start..=x.end)
            .filter(|x| is_invalid(false, *x))
            .sum::<usize>()
    );
}
