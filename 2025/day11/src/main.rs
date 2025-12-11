use std::collections::{HashMap, HashSet};

struct PathFinder<'a> {
    device_map: &'a HashMap<&'a str, Vec<&'a str>>,
    required_nodes: HashMap<&'a str, u8>,
    cache: HashMap<(&'a str, u8), usize>,
}

impl<'a> PathFinder<'a> {
    fn new(device_map: &'a HashMap<&'a str, Vec<&'a str>>, required_nodes: &[&'a str]) -> Self {
        Self {
            device_map,
            required_nodes: required_nodes
                .iter()
                .enumerate()
                .map(|(i, &name)| (name, i as u8))
                .collect(),
            cache: HashMap::new(),
        }
    }

    fn paths(&mut self, start: &'a str, target: &'a str) -> usize {
        let mut count = 0;
        self.dfs(start, start, target, 0, &mut [start].into(), &mut count);
        count
    }

    fn dfs(
        &mut self,
        start: &'a str,
        current: &'a str,
        target: &'a str,
        required_bits: u8,
        visited: &mut HashSet<&'a str>,
        count: &mut usize,
    ) {
        let key = (current, required_bits);
        if let Some(cached) = self.cache.get(&key) {
            *count += *cached;
            return;
        }

        let original_count = *count;
        if let Some(connections) = self.device_map.get(current) {
            for next in connections {
                if *next == target {
                    if self.required_nodes.is_empty()
                        || required_bits == (1 << self.required_nodes.len()) - 1
                    {
                        *count += 1;
                    }
                    continue;
                }

                if visited.insert(next) {
                    self.dfs(
                        start,
                        next,
                        target,
                        if let Some(&bit) = self.required_nodes.get(next) {
                            required_bits | 1 << bit
                        } else {
                            required_bits
                        },
                        visited,
                        count,
                    );
                    visited.remove(next);
                }
            }
        }
        self.cache.insert(key, *count - original_count);
    }
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let device_map = input
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split(':').collect();
            (parts[0], parts[1].split_whitespace().collect())
        })
        .collect();

    println!(
        "The answer to the first part is: {}",
        PathFinder::new(&device_map, &[]).paths("you", "out")
    );

    println!(
        "The answer to the second part is: {}",
        PathFinder::new(&device_map, &["dac", "fft"]).paths("svr", "out")
    );
}
