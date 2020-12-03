use std::{fs::File, io::BufRead, io::BufReader, iter::FromIterator};

#[derive(Debug, Copy, Clone, PartialEq)]
enum Token {
    Empty,
    Tree,
}

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '.' => Token::Empty,
            '#' => Token::Tree,
            _ => panic!("unexpected token"),
        }
    }
}

#[derive(Debug)]
struct Map {
    tokens: Vec<Vec<Token>>,
}

impl Map {
    fn token_at(&self, pos: &Position) -> Option<Token> {
        if pos.y >= self.tokens.len() {
            None
        } else {
            let width = self.tokens[0].len();
            Some(self.tokens[pos.y][pos.x % width])
        }
    }
}

impl FromIterator<Vec<Token>> for Map {
    fn from_iter<T: IntoIterator<Item = Vec<Token>>>(iter: T) -> Self {
        Map {
            tokens: iter.into_iter().collect(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Position {
    x: usize,
    y: usize,
}

#[derive(Copy, Clone)]
struct StepSequence {
    x: usize,
    y: usize,
    x_step: usize,
    y_step: usize,
}

impl StepSequence {
    fn new(x: usize, y: usize) -> StepSequence {
        StepSequence {
            x: 0,
            y: 0,
            x_step: x,
            y_step: y,
        }
    }
}

impl Iterator for StepSequence {
    type Item = Position;
    fn next(&mut self) -> Option<Position> {
        let p = Position {
            x: self.x,
            y: self.y,
        };

        self.x += self.x_step;
        self.y += self.y_step;

        Some(p)
    }
}

fn parse_line<T: AsRef<str>>(line: T) -> Vec<Token> {
    line.as_ref().chars().map(Token::from).collect()
}

fn count_trees(map: &Map, sequence: StepSequence) -> usize {
    sequence
        .map(|x| map.token_at(&x))
        .take_while(|x| x.is_some())
        .filter(|x| x.unwrap() == Token::Tree)
        .count()
}

fn main() {
    let file = File::open("input.txt").expect("Input file not found");
    let map = BufReader::new(file)
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_line)
        .collect::<Map>();

    println!(
        "The answer to the first part is: {}",
        count_trees(&map, StepSequence::new(3, 1))
    );

    let sequences = vec![
        StepSequence::new(1, 1),
        StepSequence::new(3, 1),
        StepSequence::new(5, 1),
        StepSequence::new(7, 1),
        StepSequence::new(1, 2),
    ];

    let mul_trees = sequences
        .iter()
        .map(|x| count_trees(&map, *x))
        .fold(1, |acc, x| acc * x);

    println!("The answer to the second part is: {:?}", mul_trees);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_step_sequence() {
        let steps = StepSequence::new(3, 1).take(3).collect::<Vec<Position>>();

        assert_eq!(Position { x: 0, y: 0 }, steps[0]);
        assert_eq!(Position { x: 3, y: 1 }, steps[1]);
        assert_eq!(Position { x: 6, y: 2 }, steps[2]);
    }
}
