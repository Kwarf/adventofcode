use std::{
    fs::File,
    io::{BufRead, BufReader},
};

type Sequence = Vec<isize>;

#[derive(Clone, Copy)]
enum Direction {
    Forward,
    Backward,
}

fn next(sequence: &Sequence, direction: Direction) -> isize {
    if sequence.iter().all(|x| *x == 0) {
        return 0;
    }

    let next = next(
        &sequence.windows(2).map(|xs| xs[1] - xs[0]).collect(),
        direction,
    );

    match direction {
        Direction::Forward => sequence.last().unwrap() + next,
        Direction::Backward => sequence.first().unwrap() - next,
    }
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let sequences: Vec<Sequence> = BufReader::new(file)
        .lines()
        .map(|line| {
            line.unwrap()
                .split_ascii_whitespace()
                .map(|x| x.parse::<isize>().unwrap())
                .collect::<Sequence>()
        })
        .collect();

    println!(
        "The answer to the first part is: {}",
        sequences
            .iter()
            .fold(0, |acc, x| acc + next(x, Direction::Forward))
    );

    println!(
        "The answer to the second part is: {}",
        sequences
            .iter()
            .fold(0, |acc, x| acc + next(x, Direction::Backward))
    );
}
