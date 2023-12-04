use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

fn parse_line(line: &str) -> (Vec<usize>, Vec<usize>) {
    let (winning, numbers) =
        line.split_ascii_whitespace()
            .fold(
                (None::<Vec<usize>>, None::<Vec<usize>>),
                |acc, x| match acc {
                    (Some(cards), Some(mut numbers)) => {
                        numbers.push(x.parse::<usize>().unwrap());
                        (Some(cards), Some(numbers))
                    }
                    (Some(cards), None) if x == "|" => (Some(cards), Some(Vec::new())),
                    (Some(mut cards), None) => {
                        cards.push(x.parse::<usize>().unwrap());
                        (Some(cards), None)
                    }
                    (None, None) if x.ends_with(":") => (Some(Vec::new()), None),
                    _ => (None, None),
                },
            );

    (winning.unwrap(), numbers.unwrap())
}

fn calculate_worth((winning, numbers): (Vec<usize>, Vec<usize>)) -> u32 {
    let a: HashSet<&usize> = HashSet::from_iter(winning.iter());
    let b: HashSet<&usize> = HashSet::from_iter(numbers.iter());

    2u32.pow(a.intersection(&b).count() as u32 - 1)
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let cards = BufReader::new(file)
        .lines()
        .map(|x| parse_line(&x.unwrap()))
        .collect::<Vec<(Vec<usize>, Vec<usize>)>>();

    println!(
        "The answer to the first part is: {}",
        cards.into_iter().map(calculate_worth).sum::<u32>()
    );
}
