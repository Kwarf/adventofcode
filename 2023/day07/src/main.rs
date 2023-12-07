use std::{
    cmp::Ordering,
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Card {
    value: char,
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

#[derive(Debug)]
struct Hand {
    cards: Vec<Card>,
    bid: usize,
}

impl Hand {
    fn get_type(&self, extra_rules: Option<fn(&mut HashMap<&Card, i32>)>) -> Type {
        let mut map = self.cards.iter().fold(HashMap::new(), |mut acc, x| {
            acc.entry(x).and_modify(|cnt| *cnt += 1).or_insert(1);
            acc
        });

        if let Some(extra_rules) = extra_rules {
            extra_rules(&mut map);
        }

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

fn compare_cards(a: &[Card], b: &[Card], rankings: &[char; 13]) -> Ordering {
    for i in 0..a.len() {
        let a = rankings.iter().position(|x| x == &a[i].value);
        let b = rankings.iter().position(|x| x == &b[i].value);
        match b.cmp(&a) {
            std::cmp::Ordering::Equal => (),
            ord => return ord,
        }
    }

    std::cmp::Ordering::Equal
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let mut hands = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap().parse().unwrap())
        .collect::<Vec<Hand>>();

    hands.sort_by(|a, b| {
        match a.get_type(None).cmp(&b.get_type(None)) {
            std::cmp::Ordering::Equal => (),
            ord => return ord,
        }

        compare_cards(
            &a.cards,
            &b.cards,
            &[
                'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2',
            ],
        )
    });

    println!(
        "The answer to the first part is: {}",
        hands
            .iter()
            .enumerate()
            .map(|(i, x)| (i + 1) * x.bid)
            .sum::<usize>()
    );

    hands.sort_by(|a, b| {
        let extra_rules = |map: &mut HashMap<&Card, i32>| {
            if let Some(n) = map.remove(&Card { value: 'J' }) {
                let foo = map
                    .iter()
                    .max_by(|a, b| a.1.cmp(&b.1))
                    .map(|x| *x.0)
                    .unwrap_or(&Card { value: 'A' });
                map.entry(&foo).and_modify(|cnt| *cnt += n).or_insert(n);
            }
        };

        match a
            .get_type(Some(extra_rules))
            .cmp(&b.get_type(Some(extra_rules)))
        {
            std::cmp::Ordering::Equal => (),
            ord => return ord,
        }

        compare_cards(
            &a.cards,
            &b.cards,
            &[
                'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J',
            ],
        )
    });

    println!(
        "The answer to the second part is: {}",
        hands
            .iter()
            .enumerate()
            .map(|(i, x)| (i + 1) * x.bid)
            .sum::<usize>()
    );
}
