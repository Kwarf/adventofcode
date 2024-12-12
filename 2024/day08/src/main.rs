use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    fs::read_to_string,
};

struct Map {
    width: usize,
    height: usize,
    antennas: HashMap<(u8, u8), char>,
}

impl Display for Map {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                let pos = (x as u8, y as u8);
                if let Some(antenna) = self.antennas.get(&pos) {
                    write!(f, "{}", antenna)?;
                } else {
                    write!(f, "{}", if self.is_antinode(pos) { '#' } else { '.' })?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Map {
    fn positions<'a>(&'a self) -> impl Iterator<Item = (u8, u8)> + 'a {
        (0..self.width as u8).flat_map(|x| (0..self.height as u8).map(move |y| (x, y)))
    }

    fn is_antinode(&self, pos: (u8, u8)) -> bool {
        self.antennas
            .iter()
            .fold(HashMap::new(), |mut map, (p, antenna)| {
                map.entry(*antenna).or_insert_with(Vec::new).push(*p);
                map
            })
            .iter()
            .any(|(_, antennas)| {
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

fn distance(a: (u8, u8), b: (u8, u8)) -> u8 {
    (a.0 as i8 - b.0 as i8).abs() as u8 + (a.1 as i8 - b.1 as i8).abs() as u8
}

fn is_collinear(positions: &[(u8, u8)]) -> bool {
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
                map.antennas.insert((x as u8, y as u8), c);
            }
            map
        },
    );

    println!(
        "The answer to the first part is: {}",
        map.positions().filter(|&pos| map.is_antinode(pos)).count()
    );
}
