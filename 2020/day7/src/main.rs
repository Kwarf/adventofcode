use std::{
    collections::{HashMap, HashSet},
    fs, iter,
    str::FromStr,
};

use regex::Regex;

#[derive(Debug)]
struct BagRule {
    color: String,
    content: HashMap<String, i32>,
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
                        x.get(1)?.as_str().parse::<i32>().ok()?,
                    ))
                })
                .collect::<HashMap<String, i32>>(),
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

fn main() {
    let rules = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .lines()
        .filter_map(|x| x.parse::<BagRule>().ok())
        .collect::<Vec<BagRule>>();

    println!(
        "The answer to the first part is: {}",
        part_one("shiny gold", &rules)
            .iter()
            .collect::<HashSet<&String>>()
            .len()
    );
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    const INPUT: &'static str = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.";

    #[test]
    fn test_part_one() {
        let rules = INPUT
            .lines()
            .filter_map(|x| x.parse::<BagRule>().ok())
            .collect::<Vec<BagRule>>();

        let result = part_one("shiny gold", &rules)
            .iter()
            .collect::<HashSet<&String>>()
            .len();

        assert_eq!(4, result);
    }
}
