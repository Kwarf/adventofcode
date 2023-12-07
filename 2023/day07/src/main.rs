use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Card {
    value: char,
}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        const RANKINGS: [char; 13] = [
            'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2',
        ];
        let a = RANKINGS.iter().position(|x| x == &self.value);
        let b = RANKINGS.iter().position(|x| x == &other.value);
        b.cmp(&a)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Type {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

#[derive(Debug, Eq)]
struct Hand {
    cards: Vec<Card>,
    bid: usize,
}

impl Hand {
    fn get_type(&self) -> Type {
        let map = self.cards.iter().fold(HashMap::new(), |mut acc, x| {
            acc.entry(x).and_modify(|cnt| *cnt += 1).or_insert(1);
            acc
        });

        match map.iter().max_by(|a, b| a.1.cmp(&b.1)) {
            Some((_, 5)) => Type::FiveOfAKind,
            Some((_, 4)) => Type::FourOfAKind,
            Some((_, 3)) if map.len() == 2 => Type::FullHouse,
            Some((_, 3)) => Type::ThreeOfAKind,
            Some((_, 2)) if map.len() == 3 => Type::TwoPair,
            Some((_, 2)) => Type::OnePair,
            Some((_, 1)) => Type::HighCard,
            _ => panic!(),
        }
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.get_type().cmp(&other.get_type()) {
            std::cmp::Ordering::Equal => (),
            ord => return ord,
        }

        for i in 0..self.cards.len() {
            match self.cards[i].cmp(&other.cards[i]) {
                std::cmp::Ordering::Equal => (),
                ord => return ord,
            }
        }

        std::cmp::Ordering::Equal
    }
}

impl FromStr for Hand {
    type Err = ();

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let [cards, bid] = line.split_ascii_whitespace().collect::<Vec<&str>>()[..2] else {
            panic!();
        };

        Ok(Hand {
            cards: cards.chars().map(|x| Card { value: x }).collect(),
            bid: bid.parse().unwrap(),
        })
    }
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let mut hands = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap().parse().unwrap())
        .collect::<Vec<Hand>>();

    hands.sort();

    println!(
        "The answer to the first part is: {}",
        hands
            .iter()
            .enumerate()
            .map(|(i, x)| (i + 1) * x.bid)
            .sum::<usize>()
    );
}
