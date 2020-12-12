use std::{
    collections::{HashMap, HashSet},
    convert::{TryFrom, TryInto},
    fs, iter,
};

use regex::Regex;

#[derive(Debug, Clone)]
struct BagRule<'a> {
    color: &'a str,
    content: HashMap<&'a str, usize>,
}

impl<'a> TryFrom<&'a str> for BagRule<'a> {
    type Error = &'static str;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let mut split = s.split(" bags contain ");
        Ok(BagRule {
            color: split.next().ok_or_else(|| "could not parse color")?,
            content: Regex::new(r"(\d+) ([^,.]*) bags*")
                .unwrap()
                .captures_iter(
                    split
                        .next()
                        .ok_or_else(|| "could not parse contained bags")?,
                )
                .filter_map(|x| Some((x.get(2)?.as_str(), x.get(1)?.as_str().parse().ok()?)))
                .collect::<HashMap<&'a str, usize>>(),
        })
    }
}

fn part_one<'a>(color: &'a str, rules: &'a [BagRule]) -> Box<dyn Iterator<Item = &'a str> + 'a> {
    Box::new(
        rules
            .iter()
            .filter(move |&x| x.content.contains_key(color))
            .flat_map(move |x| iter::once(x.color).chain(part_one(x.color, rules))),
    )
}

fn part_two(color: &str, num: usize, rules: &HashMap<&str, BagRule>) -> usize {
    rules
        .get(color)
        .expect("attempted to look up non-existent color")
        .content
        .iter()
        .fold(num, |acc, (&x, n)| acc + part_two(x, num * n, rules))
}

fn main() -> Result<(), &'static str> {
    let input = fs::read_to_string("input.txt").expect("Input file not found");
    let rules = input
        .lines()
        .map(|s| s.try_into().and_then(|x: BagRule| Ok((x.color, x))))
        .collect::<Result<HashMap<&str, BagRule>, &'static str>>()?;

    println!(
        "The answer to the first part is: {}",
        part_one(
            "shiny gold",
            &rules.values().cloned().collect::<Vec<BagRule>>()
        )
        .collect::<HashSet<&str>>()
        .len()
    );

    println!(
        "The answer to the second part is: {}",
        part_two("shiny gold", 1, &rules) - 1
    );

    Ok(())
}
