use std::{
    collections::HashSet,
    fs::File,
    hash::Hash,
    io::{BufRead, BufReader},
};

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref REGEX: Regex = Regex::new(r"(\d+|[^\.])").unwrap();
}

#[derive(PartialEq, Eq, Hash)]
enum Input {
    Number {
        position: ((i32, i32), i32),
        value: usize,
    },
    Symbol {
        position: (i32, i32),
    },
}
fn parse_line(row: i32, line: &str) -> Vec<Input> {
    REGEX
        .find_iter(line)
        .map(move |x| {
            if x.as_str().chars().next().unwrap().is_digit(10) {
                Input::Number {
                    position: ((x.start() as i32, x.end() as i32), row),
                    value: x.as_str().parse().unwrap(),
                }
            } else {
                Input::Symbol {
                    position: (x.start() as i32, row),
                }
            }
        })
        .collect()
}

fn adjacent_positions<'a>(input: &'a Input) -> impl Iterator<Item = (i32, i32)> + 'a {
    match input {
        Input::Symbol { .. } => panic!(),
        Input::Number {
            position: ((x1, x2), y),
            ..
        } => (x1 - 1..x2 + 1)
            .map(move |x| (x, *y - 1))
            .chain((x1 - 1..x2 + 1).map(move |x| (x, *y + 1)))
            .chain([(x1 - 1, *y), (*x2, *y)]),
    }
}

fn is_part_number(symbols: &HashSet<Input>, number: &Input) -> bool {
    match number {
        Input::Symbol { .. } => panic!(),
        Input::Number { .. } => {
            adjacent_positions(number).any(|x| symbols.contains(&Input::Symbol { position: x }))
        }
    }
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let (numbers, symbols): (Vec<Input>, Vec<Input>) = reader
        .lines()
        .enumerate()
        .flat_map(|(y, line)| parse_line(y as i32, &line.unwrap()))
        .partition(|x| match x {
            Input::Number { .. } => true,
            Input::Symbol { .. } => false,
        });

    let symbols: HashSet<Input> = HashSet::from_iter(symbols.into_iter());

    println!(
        "The answer to the first part is: {}",
        numbers
            .iter()
            .filter(|x| is_part_number(&symbols, x))
            .map(|x| match x {
                Input::Symbol { .. } => panic!(),
                Input::Number { value, .. } => value,
            })
            .sum::<usize>()
    );
}
