use std::{
    cmp::min,
    fs::File,
    io::{BufRead, BufReader},
};

use rayon::{iter::ParallelIterator, slice::ParallelSlice};

#[derive(Debug)]
struct Map {
    from: String,
    to: String,
    values: Vec<(u64, u64, u64)>,
}

impl Map {
    fn find(&self, value: u64) -> Option<u64> {
        self.values
            .iter()
            .find(|values| value >= values.1 && value < values.1 + values.2)
            .and_then(|x| Some(value - x.1 + x.0))
    }
}

fn parse_map(lines: &mut impl Iterator<Item = String>) -> Option<Map> {
    if let Some(block_start) = lines.skip_while(|x| x.is_empty()).next() {
        let [ref from, ref to] = block_start
            .trim_end_matches(" map:")
            .to_owned()
            .split("-to-")
            .map(|x| x.to_owned())
            .collect::<Vec<String>>()[..2]
        else {
            panic!("Bad input")
        };

        let mut values = lines
            .take_while(|x| !x.is_empty())
            .map(|x| {
                let vs = x
                    .split_ascii_whitespace()
                    .map(|x| x.parse().unwrap())
                    .collect::<Vec<u64>>();
                (vs[0], vs[1], vs[2])
            })
            .collect::<Vec<(u64, u64, u64)>>();

        values.sort_by(|(_, a, _), (_, b, _)| b.partial_cmp(&a).unwrap());

        Some(Map {
            from: from.clone(),
            to: to.clone(),
            values,
        })
    } else {
        None
    }
}

fn parse(lines: &mut impl Iterator<Item = String>) -> (Vec<u64>, Vec<Map>) {
    let seeds = lines
        .next()
        .unwrap()
        .split(':')
        .last()
        .unwrap()
        .trim()
        .split_ascii_whitespace()
        .map(|x| x.parse::<u64>().unwrap())
        .collect::<Vec<u64>>();

    let mut maps = Vec::new();
    while let Some(map) = parse_map(lines) {
        maps.push(map);
    }
    (seeds, maps)
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let mut reader = BufReader::new(file).lines().map(|x| x.unwrap());
    let (seeds, maps) = parse(&mut reader);

    println!(
        "The answer to the first part is: {}",
        seeds
            .clone()
            .into_iter()
            .map(|mut x| {
                let mut current = "seed";
                while current != "location" {
                    let map = maps.iter().find(|x| x.from == current).unwrap();
                    x = map.find(x).unwrap_or(x);
                    current = &map.to;
                }
                x
            })
            .min()
            .unwrap()
    );

    println!(
        "The answer to the second part is: {}",
        seeds
            .par_chunks_exact(2)
            .map(|pair| {
                let mut lowest = u64::MAX;
                for mut x in pair[0]..pair[0] + pair[1] {
                    let mut current = "seed";
                    while current != "location" {
                        let map = maps.iter().find(|x| x.from == current).unwrap();
                        x = map.find(x).unwrap_or(x);
                        current = &map.to;
                    }
                    lowest = min(lowest, x);
                }
                lowest
            })
            .min()
            .unwrap()
    );
}
