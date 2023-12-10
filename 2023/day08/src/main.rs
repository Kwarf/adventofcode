use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use num::integer::lcm;

fn parse_line(line: &str) -> (String, (String, String)) {
    let mut split = line.split(" = ");
    let from = split.next().unwrap();
    let to = split
        .next()
        .unwrap()
        .to_owned()
        .replace("(", "")
        .replace(")", "");
    let to = to.split(", ").collect::<Vec<&str>>();
    (from.into(), (to[0].into(), to[1].into()))
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let mut lines = BufReader::new(file).lines();

    let first_line = lines.next().unwrap().unwrap();
    let instructions = first_line.chars().cycle();

    let nodes: HashMap<String, (String, String)> =
        lines.skip(1).map(|x| parse_line(&x.unwrap())).collect();

    let find = |start: &str, to: fn(&str) -> bool| {
        instructions
            .clone()
            .into_iter()
            .fold_while((0u64, start), |(steps, current), instruction| {
                if to(&current) && steps > 0 {
                    Done((steps, current))
                } else {
                    match instruction {
                        'L' => Continue((steps + 1, &nodes[current].0)),
                        'R' => Continue((steps + 1, &nodes[current].1)),
                        _ => panic!(),
                    }
                }
            })
            .into_inner()
            .0
    };

    println!(
        "The answer to the first part is: {}",
        find("AAA", |x| x == "ZZZ")
    );

    println!(
        "The answer to the second part is: {}",
        nodes
            .keys()
            .filter(|x| x.ends_with("Z"))
            .map(|x| find(&x, |t| t.ends_with("Z")))
            .reduce(lcm)
            .unwrap()
    );
}
