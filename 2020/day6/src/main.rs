use std::{collections::HashSet, fs};

use bit_set::BitSet;

fn parse_group_p2(group: &str) -> usize {
    group
        .lines()
        .map(|x| x.chars().map(|x| x as usize - 96).collect())
        .fold((1..27).collect::<BitSet>(), |acc, x| {
            let mut ix = acc.clone();
            ix.intersect_with(&x);
            ix
        })
        .iter()
        .count()
}

fn main() {
    let groups = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .split("\n\n")
        .map(|x| x.to_owned())
        .collect::<Vec<String>>();

    println!(
        "The answer to the first part is: {}",
        groups
            .iter()
            .map(|x| x.chars().filter(|&x| x != '\n').collect::<HashSet<char>>())
            .fold(0, |acc, x| acc + x.len())
    );

    println!(
        "The answer to the second part is: {}",
        groups.iter().map(|x| parse_group_p2(x)).sum::<usize>()
    );
}
