use std::collections::{HashSet, VecDeque};

#[derive(Debug)]
struct State {
    lights: Vec<bool>,
}

#[derive(Debug)]
struct Schematic {
    wiring: HashSet<usize>,
}

#[derive(Debug)]
struct Machine {
    desired_state: State,
    schematics: Vec<Schematic>,
}

impl Machine {
    fn configure(&self) -> usize {
        let initial_state = vec![false; self.desired_state.lights.len()];
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back((initial_state.clone(), 0));
        visited.insert(initial_state);

        while let Some((current_state, presses)) = queue.pop_front() {
            for schematic in &self.schematics {
                let mut new_state = current_state.clone();
                for &light in &schematic.wiring {
                    if light < new_state.len() {
                        new_state[light] = !new_state[light];
                    }
                }

                if new_state == self.desired_state.lights {
                    return presses + 1;
                }

                if !visited.contains(&new_state) {
                    visited.insert(new_state.clone());
                    queue.push_back((new_state, presses + 1));
                }
            }
        }
        unreachable!()
    }
}

fn parse_line(line: &str) -> Machine {
    let parts: Vec<&str> = line.split_whitespace().collect();

    let desired_state = State {
        lights: parts[0]
            .trim_matches(&['[', ']'][..])
            .chars()
            .map(|c| c == '#')
            .collect(),
    };

    let schematics = parts[1..]
        .iter()
        .filter(|part| part.starts_with('(') && part.ends_with(')'))
        .map(|part| Schematic {
            wiring: part
                .trim_matches(&['(', ')'][..])
                .split(',')
                .filter_map(|s| s.parse::<usize>().ok())
                .collect(),
        })
        .collect();

    Machine {
        desired_state,
        schematics,
    }
}

fn main() {
    let machines = std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(parse_line)
        .collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        machines.iter().map(|m| m.configure()).sum::<usize>()
    );
}
