use std::{collections::HashSet, fs::read_to_string, hash::Hash, ops::Add};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
struct Vec2(i32, i32);

impl Add<Vec2> for Vec2 {
    type Output = Vec2;

    fn add(self, rhs: Vec2) -> Vec2 {
        Vec2(self.0 + rhs.0, self.1 + rhs.1)
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

    fn with_obstacle(&self, position: &Vec2) -> Map {
        let mut obstacles = self.obstacles.clone();
        obstacles.insert(*position);
        Map {
            obstacles,
            size: self.size,
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
struct Guard {
    position: Vec2,
    direction: Vec2,
}

impl Guard {
    fn moved(&self) -> Guard {
        Guard {
            position: self.position + self.direction,
            direction: self.direction,
        }
    }

    fn turned(&self) -> Guard {
        Guard {
            position: self.position,
            direction: Vec2(-self.direction.1, self.direction.0),
        }
    }
}

#[derive(PartialEq)]
enum Result {
    Exited(HashSet<Guard>),
    Loop,
}

fn walk(mut visited: HashSet<Guard>, map: &Map, guard: Guard) -> Result {
    if !visited.insert(guard) {
        return Result::Loop;
    }

    if !map.is_in_bounds(&guard.moved().position) {
        return Result::Exited(visited);
    }

    walk(
        visited,
        map,
        if map.is_obstacle(&guard.moved().position) {
            guard.turned()
        } else {
            guard.moved()
        },
    )
}

fn place_obstacles(visited: &HashSet<Guard>, map: &Map, initial: &Guard) -> HashSet<Vec2> {
    visited
        .iter()
        .map(|visited| visited.moved().position)
        .filter(|p| map.is_in_bounds(p) && !map.is_obstacle(p))
        .filter_map(
            |p| match walk(HashSet::new(), &map.with_obstacle(&p), initial.clone()) {
                Result::Loop => Some(p),
                Result::Exited(_) => None,
            },
        )
        .collect()
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

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let lines = input.lines().collect::<Vec<_>>();
    let map = parse_map(&lines);
    let guard = Guard {
        position: find_chars(&lines, '^')[0],
        direction: Vec2(0, -1),
    };

    let result = walk(HashSet::new(), &map, guard);

    println!(
        "The answer to the first part is: {}",
        match &result {
            Result::Exited(visited) => visited
                .iter()
                .map(|x| x.position)
                .collect::<HashSet<_>>()
                .len(),
            Result::Loop => unreachable!(),
        }
    );

    println!(
        "The answer to the second part is: {}",
        match &result {
            Result::Exited(visited) => place_obstacles(visited, &map, &guard).len(),
            Result::Loop => unreachable!(),
        }
    );
}
