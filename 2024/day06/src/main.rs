use std::{collections::HashSet, fs::read_to_string, hash::Hash, ops::Add};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
struct Vec2(i32, i32);

impl Add<Vec2> for Vec2 {
    type Output = Vec2;

    fn add(self, rhs: Vec2) -> Vec2 {
        Vec2(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl Vec2 {
    fn rotated_clockwise(&self) -> Vec2 {
        Vec2(-self.1, self.0)
    }
}

struct Map {
    obstacles: HashSet<Vec2>,
    size: Vec2,
}

impl Map {
    fn is_in_bounds(&self, position: &Vec2) -> bool {
        position.0 >= 0 && position.0 < self.size.0 && position.1 >= 0 && position.1 < self.size.1
    }

    fn is_obstacle(&self, position: &Vec2) -> bool {
        self.obstacles.contains(&position)
    }
}

fn find_chars(input: &[&str], c: char) -> Vec<Vec2> {
    input
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, ch)| {
                if ch == c {
                    Some(Vec2(x as i32, y as i32))
                } else {
                    None
                }
            })
        })
        .collect()
}

fn parse_map(input: &[&str]) -> Map {
    Map {
        obstacles: find_chars(input, '#').into_iter().collect(),
        size: Vec2(input[0].len() as i32, input.len() as i32),
    }
}

fn walk(
    mut visited: HashSet<Vec2>,
    map: &Map,
    position: Vec2,
    direction: Vec2,
) -> Option<HashSet<Vec2>> {
    visited.insert(position);
    if !map.is_in_bounds(&(position + direction)) {
        return Some(visited);
    }

    let (next, direction) = if map.is_obstacle(&(position + direction)) {
        (
            position + direction.rotated_clockwise(),
            direction.rotated_clockwise(),
        )
    } else {
        (position + direction, direction)
    };

    walk(visited, map, next, direction)
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let lines = input.lines().collect::<Vec<_>>();
    let map = parse_map(&lines);
    let guard_position = find_chars(&lines, '^')[0];

    println!(
        "The answer to the first part is: {}",
        walk(HashSet::new(), &map, guard_position, Vec2(0, -1))
            .unwrap()
            .len()
    );
}
