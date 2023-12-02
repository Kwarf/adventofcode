use std::{
    cmp::max,
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    ops::Index,
};

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref REGEX: Regex = Regex::new(r"(\d+)\s(\w+)").unwrap();
}

#[derive(PartialEq, Eq, Hash)]
enum Color {
    Red,
    Green,
    Blue,
}

type Game = Vec<(u32, Color)>;

fn parse_game(line: &str) -> Game {
    REGEX
        .captures_iter(line)
        .map(|x| x.extract())
        .map(|(_, [count, color])| match color {
            "red" => (count.parse().unwrap(), Color::Red),
            "green" => (count.parse().unwrap(), Color::Green),
            "blue" => (count.parse().unwrap(), Color::Blue),
            _ => panic!(),
        })
        .collect()
}

fn is_possible<'a>(bag: &impl Index<&'a Color, Output = u32>, game: &'a Game) -> bool {
    game.iter().all(|x| bag[&x.1] >= x.0)
}

fn minimum_required_power(game: &Game) -> u32 {
    game.iter()
        .fold([0, 0, 0], |[r, g, b], (n, color)| match color {
            Color::Red => [max(r, *n), g, b],
            Color::Green => [r, max(g, *n), b],
            Color::Blue => [r, g, max(b, *n)],
        })
        .into_iter()
        .product()
}

fn main() {
    let bag = HashMap::from([(Color::Red, 12), (Color::Green, 13), (Color::Blue, 14)]);

    let file = File::open("input.txt").unwrap();
    let games = BufReader::new(file)
        .lines()
        .map(|x| parse_game(&x.unwrap()))
        .collect::<Vec<Game>>();

    println!(
        "The answer to the first part is: {}",
        games
            .iter()
            .enumerate()
            .filter_map(|(i, game)| if is_possible(&bag, game) {
                Some(i + 1)
            } else {
                None
            })
            .sum::<usize>()
    );

    println!(
        "The answer to the second part is: {}",
        games
            .iter()
            .map(minimum_required_power)
            .sum::<u32>()
    );
}
