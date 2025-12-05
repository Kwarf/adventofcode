use std::{collections::HashSet, iter::once, ops::RangeInclusive};

fn parse_input(input: &str) -> (HashSet<RangeInclusive<usize>>, Vec<usize>) {
    let mut input_parts = input.split("\n\n");
    let ranges = input_parts.next().unwrap();
    let available_ids = input_parts.next().unwrap();
    (
        ranges.lines().fold(HashSet::new(), |mut set, line| {
            let (start, end) = line
                .split_once('-')
                .map(|(s, e)| (s.parse().unwrap(), e.parse().unwrap()))
                .unwrap();

            let overlapping: Vec<_> = set
                .iter()
                .filter(|r| start <= *r.end() && end >= *r.start())
                .cloned()
                .collect();

            if overlapping.is_empty() {
                set.insert(start..=end);
            } else {
                let new_start = overlapping
                    .iter()
                    .map(|r| *r.start())
                    .chain(once(start))
                    .min()
                    .unwrap();

                let new_end = overlapping
                    .iter()
                    .map(|r| *r.end())
                    .chain(once(end))
                    .max()
                    .unwrap();

                set.retain(|r| !overlapping.contains(r));
                set.insert(new_start..=new_end);
            }
            set
        }),
        available_ids
            .lines()
            .map(|line| line.parse().unwrap())
            .collect(),
    )
}

fn main() {
    let (database, ids) = parse_input(&std::fs::read_to_string("input.txt").unwrap());

    println!(
        "The answer to the first part is: {}",
        ids.iter()
            .filter(|id| database.iter().any(|range| range.contains(id)))
            .count()
    );

    println!(
        "The answer to the second part is: {}",
        database
            .iter()
            .map(|range| range.end() - range.start() + 1)
            .sum::<usize>()
    );
}
