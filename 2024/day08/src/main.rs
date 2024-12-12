use std::{collections::HashMap, fs::read_to_string};

use itertools::Itertools;

struct Map {
    width: usize,
    height: usize,
    antennas: HashMap<char, Vec<(i8, i8)>>,
}

impl Map {
    fn positions<'a>(&'a self) -> impl Iterator<Item = (i8, i8)> + 'a {
        (0..self.width as i8).flat_map(|x| (0..self.height as i8).map(move |y| (x, y)))
    }

    fn is_out_of_bounds(&self, pos: (i8, i8)) -> bool {
        pos.0 < 0 || pos.0 >= self.width as i8 || pos.1 < 0 || pos.1 >= self.height as i8
    }

    fn is_antinode(&self, pos: (i8, i8)) -> bool {
        self.antennas.iter().any(|(_, antennas)| {
            antennas.iter().any(|antenna_pos| {
                let d = distance(*antenna_pos, pos);
                d > 0
                    && antennas.iter().any(|other_antenna_pos| {
                        d == distance(*other_antenna_pos, pos) * 2
                            && is_collinear(&[pos, *antenna_pos, *other_antenna_pos])
                    })
            })
        })
    }
}

fn distance(a: (i8, i8), b: (i8, i8)) -> i8 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn is_collinear(positions: &[(i8, i8)]) -> bool {
    if positions.len() <= 2 {
        return true;
    }

    let (x0, y0) = positions[0];
    let (x1, y1) = positions[1];
    let (dx, dy) = (x1 as i32 - x0 as i32, y1 as i32 - y0 as i32);

    positions[2..]
        .iter()
        .all(|(xi, yi)| dx * ((*yi as i32) - y0 as i32) == dy * ((*xi as i32) - x0 as i32))
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let map = input.lines().enumerate().fold(
        Map {
            width: 0,
            height: 0,
            antennas: HashMap::new(),
        },
        |mut map, (y, line)| {
            map.width = line.len();
            map.height += 1;
            for (x, c) in line.chars().enumerate().filter(|(_, c)| *c != '.') {
                map.antennas
                    .entry(c)
                    .or_insert_with(Vec::new)
                    .push((x as i8, y as i8));
            }
            map
        },
    );

    println!(
        "The answer to the first part is: {}",
        map.positions().filter(|&pos| map.is_antinode(pos)).count()
    );

    println!(
        "The answer to the second part is: {}",
        map.antennas
            .iter()
            .flat_map(|(_, antennas)| antennas.iter().permutations(2))
            .flat_map(|combination| {
                let (a, b) = (combination[0], combination[1]);
                let (dx, dy) = (b.0 - a.0, b.1 - a.1);
                (a.0..)
                    .step_by(dx as usize)
                    .zip((a.1..).step_by(dy as usize))
                    .take_while(|&pos| !map.is_out_of_bounds(pos))
            })
            .unique()
            .count()
    );
}
