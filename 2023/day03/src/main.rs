use std::{
    collections::{HashMap, HashSet},
    fs::File,
    hash::Hash,
    io::{BufRead, BufReader},
    ops::Add,
};

use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref REGEX: Regex = Regex::new(r"(\d+|[^\.])").unwrap();
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: i32,
    y: i32,
}

impl From<(i32, i32)> for Position {
    fn from(value: (i32, i32)) -> Self {
        Position {
            x: value.0,
            y: value.1,
        }
    }
}

impl Add for Position {
    type Output = Position;

    fn add(self, rhs: Self) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

struct Number {
    positions: HashSet<Position>,
    value: usize,
}

struct Symbol {
    position: Position,
    symbol: char,
}

fn parse_line(row: i32, line: &str) -> (Vec<Number>, Vec<Symbol>) {
    REGEX.find_iter(line).fold(
        (Vec::new(), Vec::new()),
        move |(mut numbers, mut symbols), x| {
            if x.as_str().chars().next().unwrap().is_digit(10) {
                numbers.push(Number {
                    positions: HashSet::from_iter(
                        (x.start()..x.end()).map(|x| (x as i32, row).into()),
                    ),
                    value: x.as_str().parse().unwrap(),
                });
            } else {
                symbols.push(Symbol {
                    position: (x.start() as i32, row).into(),
                    symbol: x.as_str().chars().next().unwrap(),
                });
            }
            (numbers, symbols)
        },
    )
}

fn adjacent_positions<'a>(input: &'a Position) -> [Position; 8] {
    [
        *input + (-1, -1).into(),
        *input + (0, -1).into(),
        *input + (1, -1).into(),
        *input + (-1, 0).into(),
        *input + (1, 0).into(),
        *input + (-1, 1).into(),
        *input + (0, 1).into(),
        *input + (1, 1).into(),
    ]
}

fn is_part_number(symbols: &HashMap<Position, Symbol>, number: &Number) -> bool {
    number
        .positions
        .iter()
        .flat_map(|x| adjacent_positions(x))
        .any(|x| symbols.contains_key(&x))
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let (numbers, symbols): (Vec<Number>, Vec<Symbol>) = reader
        .lines()
        .enumerate()
        .map(|(y, line)| parse_line(y as i32, &line.unwrap()))
        .fold(
            (Vec::new(), Vec::new()),
            |(mut numbers, mut symbols), mut x| {
                numbers.append(&mut x.0);
                symbols.append(&mut x.1);
                (numbers, symbols)
            },
        );

    let symbol_map: HashMap<Position, Symbol> =
        HashMap::from_iter(symbols.into_iter().map(|x| (x.position, x)));

    println!(
        "The answer to the first part is: {}",
        numbers
            .iter()
            .filter(|x| is_part_number(&symbol_map, x))
            .map(|x| x.value)
            .sum::<usize>()
    );

    println!(
        "The answer to the second part is: {}",
        symbol_map
            .values()
            .filter(|x| x.symbol == '*')
            .map(|x| adjacent_positions(&x.position)
                .iter()
                .flat_map(|p| numbers.iter().filter(move |n| n.positions.contains(&p)))
                .unique_by(|x| x.value)
                .collect::<Vec<&Number>>())
            .filter(|x| x.len() == 2)
            .map(|x| x.iter().map(|n| n.value).product::<usize>())
            .sum::<usize>()
    );
}
