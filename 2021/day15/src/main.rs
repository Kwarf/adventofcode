use std::{cmp::Ordering, collections::BinaryHeap, fs::read_to_string};

struct Input {
    width: usize,
    data: Vec<u8>,
}

fn wrap_add(value: u8, add: u8) -> u8 {
    match value + add {
        x if x > 9 => x - 9,
        x => x,
    }
}

fn parse_p1(input: &str) -> Input {
    Input {
        width: input.lines().next().unwrap().len(),
        data: input
            .bytes()
            .filter(|&x| x != b'\n')
            .map(|x| (x as char).to_digit(10).unwrap() as u8)
            .collect(),
    }
}

fn parse_p2(input: &str) -> Input {
    // Can't really be bothered to make this good
    const F: usize = 5;
    let dx = input.lines().next().unwrap().len();
    let sequence = input
        .lines()
        .flat_map(|l| {
            l.bytes()
                .cycle()
                .take(l.len() * F)
                .map(|x| (x as char).to_digit(10).unwrap() as u8)
                .enumerate()
                .map(|(i, x)| wrap_add(x, (i / dx) as u8))
        })
        .collect::<Vec<u8>>();

    Input {
        width: dx * 5,
        data: (0..F)
            .flat_map(|i| sequence.iter().map(move |&x| wrap_add(x, i as u8)))
            .collect(),
    }
}

#[derive(Eq, PartialEq)]
struct State {
    risk: usize,
    index: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .risk
            .cmp(&self.risk)
            .then_with(|| self.index.cmp(&other.index))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn xy_to_index(input: &Input, (x, y): (usize, usize)) -> usize {
    x + input.width * y
}

fn neighbors(input: &Input, index: usize) -> [Option<usize>; 4] {
    let (x, y) = (index % input.width, index / input.width);
    [
        if x > 0 { Some(xy_to_index(&input, (x - 1, y))) } else { None },
        if x + 1 < input.width { Some(xy_to_index(&input, (x + 1, y))) } else { None },
        if y > 0 { Some(xy_to_index(&input, (x, y - 1))) } else { None },
        if index + input.width < input.data.len() { Some(index + input.width) } else { None },
    ]
}

fn calculate_risk(input: &Input) -> usize {
    let mut risks: Vec<Option<usize>> = vec![None; input.data.len()];
    risks[0] = Some(0);

    let mut heap = BinaryHeap::new();
    heap.push(State { risk: 0, index: 0 });

    while let Some(State { risk, index }) = heap.pop() {
        if risk > risks[index].unwrap_or(usize::MAX) {
            continue;
        }

        for &n in neighbors(&input, index).iter().filter(|&x| x.is_some()) {
            let next_pos = n.unwrap();
            let next_risk: usize = risk + input.data[next_pos] as usize;

            if next_risk < risks[next_pos].unwrap_or(usize::MAX) {
                heap.push(State {
                    risk: next_risk,
                    index: next_pos,
                });
                risks[next_pos] = Some(next_risk);
            }
        }
    }

    risks.last().unwrap().unwrap()
}

fn main() {
    let input = read_to_string("input.txt").unwrap();

    println!(
        "The answer to the first part is: {}",
        calculate_risk(&parse_p1(&input))
    );

    println!(
        "The answer to the second part is: {}",
        calculate_risk(&parse_p2(&input))
    );
}
