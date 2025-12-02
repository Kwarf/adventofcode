use std::ops::Range;

fn parse_range(range_str: &str) -> Range<usize> {
    let parts: Vec<&str> = range_str.split('-').collect();
    Range {
        start: parts[0].parse().unwrap(),
        end: parts[1].parse().unwrap(),
    }
}

fn main() {
    let numbers = std::fs::read_to_string("input.txt")
        .unwrap()
        .split(',')
        .map(parse_range)
        .flat_map(|x| x.start..=x.end)
        .map(|x| x.to_string())
        .collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        numbers
            .iter()
            .filter(|x| {
                let len = x.len();
                if !len.is_multiple_of(2) {
                    return false;
                }
                let mid = len / 2;
                x[..mid] == x[mid..]
            })
            .map(|x| x.parse::<usize>().unwrap())
            .sum::<usize>()
    );

    println!(
        "The answer to the second part is: {}",
        numbers
            .iter()
            .filter(|x| {
                let len = x.len();
                for size in (1..=len / 2).filter(|x| len.is_multiple_of(*x)) {
                    let mut matched = true;
                    for i in (0..len).step_by(size) {
                        if x[i..i + size] != x[0..size] {
                            matched = false;
                            break;
                        }
                    }
                    if matched {
                        return true;
                    }
                }
                false
            })
            .map(|x| x.parse::<usize>().unwrap())
            .sum::<usize>()
    );
}
