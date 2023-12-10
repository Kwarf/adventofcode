use std::{
    fs::File,
    io::{BufRead, BufReader},
};

type Sequence = Vec<isize>;

fn next(sequence: &Sequence) -> isize {
    if sequence.iter().all(|x| *x == 0) {
        return 0;
    }

    sequence.last().unwrap() + next(&sequence.windows(2).map(|xs| xs[1] - xs[0]).collect())
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let sequences = BufReader::new(file).lines().map(|line| {
        line.unwrap()
            .split_ascii_whitespace()
            .map(|x| x.parse::<isize>().unwrap())
            .collect::<Sequence>()
    });

    println!(
        "The answer to the first part is: {}",
        sequences.map(|x| next(&x)).sum::<isize>()
    );
}
