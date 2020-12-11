use std::{
    collections::{HashMap, HashSet},
    fs, iter,
    str::FromStr,
};

use regex::Regex;

#[derive(Debug, Clone)]
struct BagRule {
    color: String,
    content: HashMap<String, usize>,
}

impl FromStr for BagRule {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(" bags contain ");
        Ok(BagRule {
            color: split.next().ok_or_else(|| ())?.to_owned(),
            content: Regex::new(r"(\d+) ([^,.]*) bags*")
                .map_err(|_| ())?
                .captures_iter(split.next().ok_or_else(|| ())?)
                .filter_map(|x| {
                    Some((
                        x.get(2)?.as_str().to_owned(),
                        x.get(1)?.as_str().parse::<usize>().ok()?,
                    ))
                })
                .collect::<HashMap<String, usize>>(),
        })
    }
}

fn part_one(color: &str, rules: &[BagRule]) -> Vec<String> {
    rules
        .iter()
        .filter(|x| x.content.contains_key(color))
        .map(|x| x.color.clone())
        .flat_map(|x| iter::once(x.clone()).chain(part_one(&x, rules)))
        .collect::<Vec<String>>()
}

fn part_two(color: &str, num: usize, rules: &HashMap<String, BagRule>) -> usize {
    rules
        .get(color)
        .unwrap()
        .content
        .iter()
        .fold(num, |acc, (x, n)| acc + part_two(&x, num * n, rules))
}

fn main() {
    let rules = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .lines()
        .filter_map(|x| x.parse::<BagRule>().ok())
        .map(|x| (x.color.clone(), x))
        .collect::<HashMap<String, BagRule>>();

    println!(
        "The answer to the first part is: {}",
        part_one(
            "shiny gold",
            &rules.values().cloned().collect::<Vec<BagRule>>()
        )
        .iter()
        .collect::<HashSet<&String>>()
        .len()
    );

    println!(
        "The answer to the second part is: {}",
        part_two("shiny gold", 1, &rules) - 1
    );
}
