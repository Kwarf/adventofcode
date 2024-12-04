use std::fs::read_to_string;

use itertools::Itertools;

struct Grid(Vec<Vec<char>>);

impl Grid {
    fn get(&self, x: usize, y: usize) -> Option<&char> {
        self.0.get(y).and_then(|row| row.get(x))
    }

    fn coordinates<'a>(&'a self) -> impl Iterator<Item = (usize, usize)> + 'a {
        (0..self.0.len()).flat_map(|y| (0..self.0[y].len()).map(move |x| (x, y)))
    }

    fn is_xmas(&self, pos: (i32, i32), vector: (i32, i32), expected: char) -> bool {
        self.get(pos.0 as usize, pos.1 as usize) == Some(&expected)
            && match expected {
                'X' => self.is_xmas((pos.0 + vector.0, pos.1 + vector.1), vector, 'M'),
                'M' => self.is_xmas((pos.0 + vector.0, pos.1 + vector.1), vector, 'A'),
                'A' => self.is_xmas((pos.0 + vector.0, pos.1 + vector.1), vector, 'S'),
                'S' => true,
                _ => unreachable!(),
            }
    }
}

fn main() {
    let input = Grid(
        read_to_string("input.txt")
            .unwrap()
            .lines()
            .map(|x| x.chars().collect::<Vec<_>>())
            .collect(),
    );

    let search_vectors = (-1..=1)
        .flat_map(|y| (-1..=1).map(move |x| (x, y)))
        .filter(|(x, y)| *x != 0 || *y != 0)
        .collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        input
            .coordinates()
            .map(|(x, y)| ((x, y), input.get(x, y)))
            .filter(|(_, c)| c == &Some(&'X'))
            .map(|((x, y), _)| {
                search_vectors
                    .iter()
                    .filter(|v| input.is_xmas((x as i32, y as i32), **v, 'X'))
                    .count()
            })
            .sum::<usize>()
    );

    println!(
        "The answer to the second part is: {}",
        input
            .coordinates()
            .map(|(x, y)| ((x as i32, y as i32), input.get(x, y)))
            .filter(|(_, c)| c == &Some(&'M'))
            .flat_map(|((x, y), _)| {
                [(-1, -1), (1, -1), (1, 1), (-1, 1)]
                    .iter()
                    .filter(|&&v| input.is_xmas((x, y), v, 'M'))
                    .map(|v| ((x + v.0), (y + v.1)))
                    .collect::<Vec<_>>()
            })
            .duplicates()
            .count()
    );
}
