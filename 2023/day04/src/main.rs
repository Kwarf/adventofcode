use std::{
    collections::{HashSet, VecDeque},
    fs::File,
    io::{BufRead, BufReader},
    ops::AddAssign,
    str::FromStr,
};

#[derive(Clone)]
struct Card {
    winnings: Vec<usize>,
    numbers: Vec<usize>,
}

impl Card {
    fn matches(&self) -> usize {
        let a: HashSet<&usize> = HashSet::from_iter(self.winnings.iter());
        let b: HashSet<&usize> = HashSet::from_iter(self.numbers.iter());
        a.intersection(&b).count()
    }
}

impl FromStr for Card {
    type Err = ();

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let (winnings, numbers) = line.split_ascii_whitespace().fold(
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

        Ok(Self {
            winnings: winnings.unwrap(),
            numbers: numbers.unwrap(),
        })
    }
}

struct Hand {
    cards: VecDeque<(usize, Card)>,
    total: usize,
}

impl Iterator for Hand {
    type Item = (usize, Card);

    fn next(&mut self) -> Option<Self::Item> {
        self.cards.pop_front()
    }
}

impl AddAssign<usize> for Hand {
    fn add_assign(&mut self, count: usize) {
        self.total += count;
        for i in 0..count {
            self.cards[i].0 += 1
        }
    }
}

impl<T> From<T> for Hand
where
    T: IntoIterator<Item = Card>,
{
    fn from(cards: T) -> Self {
        let cards = VecDeque::from_iter(cards.into_iter().map(|x| (1, x)));
        let total = cards.len();
        Hand { cards, total }
    }
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let cards = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap().parse().unwrap())
        .collect::<Vec<Card>>();

    println!(
        "The answer to the first part is: {}",
        cards
            .iter()
            .map(|x| 2u32.pow(x.matches() as u32 - 1))
            .sum::<u32>()
    );

    let mut hand: Hand = cards.into();
    while let Some((n, card)) = hand.next() {
        let matches = card.matches();
        for _ in 0..n {
            hand += matches;
        }
    }
    println!("The answer to the second part is: {}", hand.total);
}
