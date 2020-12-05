use std::{collections::HashSet, fs, str::FromStr};

#[derive(Debug, PartialEq)]
struct Seat {
    row: i32,
    column: i32,
}

impl Seat {
    fn id(&self) -> i32 {
        self.row * 8 + self.column
    }
}

impl FromStr for Seat {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Seat {
            row: BinIter::new(&s[..7], 127).last().ok_or(())?,
            column: BinIter::new(&s[7..], 7).last().ok_or(())?,
        })
    }
}

struct BinIter<'a> {
    pattern: &'a str,
    value: (i32, i32),
    step: usize,
}

impl BinIter<'_> {
    fn new<'a>(pattern: &'a str, max: i32) -> BinIter<'a> {
        BinIter {
            pattern,
            value: (0, max),
            step: 0,
        }
    }
}

impl Iterator for BinIter<'_> {
    type Item = i32;
    fn next(&mut self) -> Option<i32> {
        if self.step >= self.pattern.len() {
            return None;
        }

        self.value = match self.pattern.chars().nth(self.step) {
            Some('F') | Some('L') => (self.value.0, (self.value.0 + self.value.1) / 2),
            Some('B') | Some('R') => ((self.value.0 + self.value.1) / 2 + 1, self.value.1),
            _ => panic!("Unexpected seat pattern"),
        };
        self.step += 1;

        Some(self.value.0)
    }
}

fn main() {
    let seat_ids = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .lines()
        .filter_map(|x| x.parse::<Seat>().ok())
        .map(|x| x.id())
        .collect::<HashSet<i32>>();

    println!(
        "The answer to the first part is: {}",
        seat_ids.iter().max().unwrap()
    );

    println!(
        "The answer to the second part is: {}",
        seat_ids
            .iter()
            .filter(|&x| !seat_ids.contains(&(x + 1)) && seat_ids.contains(&(x + 2)))
            .next()
            .map(|x| x + 1)
            .unwrap()
    );
}
