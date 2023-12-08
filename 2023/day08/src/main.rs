use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;

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

    println!(
        "The answer to the first part is: {}",
        instructions
            .into_iter()
            .fold_while((0, "AAA"), |(steps, current), instruction| {
                if current == "ZZZ" {
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
    );
}
