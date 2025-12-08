use std::collections::HashSet;

use itertools::Itertools;

struct Position {
    x: usize,
    y: usize,
    z: usize,
}

impl Position {
    fn distance_to(&self, other: &Position) -> usize {
        let dx = self.x as isize - other.x as isize;
        let dy = self.y as isize - other.y as isize;
        let dz = self.z as isize - other.z as isize;
        (dx * dx + dy * dy + dz * dz) as usize
    }
}

pub struct DisjointSet {
    parent: Vec<usize>,
    size: Vec<usize>,
}

impl DisjointSet {
    pub fn new(size: usize) -> Self {
        let mut parent = Vec::with_capacity(size);
        for i in 0..size {
            parent.push(i);
        }

        DisjointSet {
            parent,
            size: vec![1; size],
        }
    }

    pub fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    pub fn union(&mut self, x: usize, y: usize) {
        let x_root = self.find(x);
        let y_root = self.find(y);

        if x_root == y_root {
            return;
        }

        if self.size[x_root] < self.size[y_root] {
            self.parent[x_root] = y_root;
            self.size[y_root] += self.size[x_root];
        } else {
            self.parent[y_root] = x_root;
            self.size[x_root] += self.size[y_root];
        }
    }

    pub fn sizes(&mut self) -> Vec<usize> {
        let mut roots = HashSet::new();
        for i in 0..self.parent.len() {
            roots.insert(self.find(i));
        }
        roots.into_iter().map(|r| self.size[r]).collect()
    }
}

fn main() {
    let boxes: Vec<_> = std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(|line| {
            let (x, y, z) = line
                .trim()
                .split(',')
                .map(|s| s.parse().unwrap())
                .collect_tuple()
                .unwrap();
            Position { x, y, z }
        })
        .collect();

    let edges: Vec<_> = boxes
        .iter()
        .enumerate()
        .flat_map(|(i, a)| {
            boxes
                .iter()
                .enumerate()
                .skip(i + 1)
                .map(move |(j, b)| (a.distance_to(b), i, j))
        })
        .sorted_by(|(a, _, _), (b, _, _)| a.cmp(b))
        .collect();

    let mut set = DisjointSet::new(boxes.len());
    for &(_, a, b) in edges.iter().take(1000) {
        set.union(a, b);
    }

    println!(
        "The answer to the first part is: {}",
        set.sizes().iter().sorted().rev().take(3).product::<usize>()
    );
}
