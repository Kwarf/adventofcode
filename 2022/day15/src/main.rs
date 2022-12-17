use rayon::prelude::*;
use regex::Regex;

#[derive(Debug, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}

impl Position {
    fn distance_to(&self, other: &Position) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

#[derive(Debug)]
struct Reading {
    origin: Position,
    closest: Position,
}

impl Reading {
    fn contains(&self, other: &Position) -> bool {
        self.origin.distance_to(other) <= self.distance()
    }

    fn distance(&self) -> i32 {
        self.origin.distance_to(&self.closest)
    }

    fn skippable(&self, other: &Position) -> i32 {
        self.distance() - self.origin.distance_to(other)
    }
}

fn find_empty(readings: &[Reading], y: i32) -> Option<Position> {
    let mut iter = 0..=4000000;
    while let Some(x) = iter.next() {
        let current = Position { x, y };
        if let Some(skips) = readings.iter().filter(|r| r.contains(&current)).map(|x| x.skippable(&current) - 1).max() {
            iter.nth(skips as usize);
        } else {
            return Some(current);
        }
    }
    None
}

fn main() {
    let (mut x_min, mut x_max) = (0, 0);
    let readings =
        Regex::new(r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
            .unwrap()
            .captures_iter(&std::fs::read_to_string("input.txt").unwrap())
            .map(|x| {
                let reading = Reading {
                    origin: Position {
                        x: x[1].parse().unwrap(),
                        y: x[2].parse().unwrap(),
                    },
                    closest: Position {
                        x: x[3].parse().unwrap(),
                        y: x[4].parse().unwrap(),
                    },
                };
                let d = reading.distance();
                x_min = x_min.min(reading.origin.x.min(reading.closest.x) - d);
                x_max = x_max.max(reading.origin.x.max(reading.closest.x) + d);
                reading
            })
            .collect::<Vec<Reading>>();

    println!(
        "The answer to the first part is: {}",
        (x_min..x_max)
            .into_par_iter()
            .map(|x| Position { x, y: 2000000 })
            .filter(|x| readings.iter().any(|r| r.closest != *x && r.contains(&x)))
            .count()
    );

    println!(
        "The answer to the second part is: {}",
        (0..=4000000)
            .into_par_iter()
            .map(|y| find_empty(&readings, y))
            .find_any(|y| y.is_some())
            .and_then(|p| p)
            .and_then(|p| Some(p.x as i64 * 4000000i64 + p.y as i64))
            .unwrap()
    );
}
