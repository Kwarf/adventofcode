use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};

// A Button is represented by the set of lights it toggles.
type Button = BTreeSet<usize>;

struct Machine {
    desired_lights: Vec<bool>,
    desired_joltages: Vec<u16>,
    patterns: HashMap<Button, Vec<Vec<Button>>>,
}

impl Machine {
    fn new(desired_lights: Vec<bool>, desired_joltages: Vec<u16>, schematic: &[Button]) -> Self {
        let patterns = Self::build_patterns(schematic);
        Self {
            desired_lights,
            desired_joltages,
            patterns,
        }
    }

    fn build_patterns(schematic: &[Button]) -> HashMap<Button, Vec<Vec<Button>>> {
        let mut patterns: HashMap<Button, Vec<Vec<Button>>> = HashMap::new();
        for num_presses in 0..=schematic.len() {
            for presses in schematic.iter().combinations(num_presses) {
                let mut pattern = Button::new();
                for button in &presses {
                    pattern = pattern.symmetric_difference(button).copied().collect();
                }
                patterns
                    .entry(pattern.clone())
                    .or_default()
                    .push(presses.into_iter().cloned().collect());
            }
        }
        patterns
    }

    fn presses_for_lights(&self) -> usize {
        self.patterns
            .get(
                &self
                    .desired_lights
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &light)| if light { Some(i) } else { None })
                    .collect(),
            )
            .and_then(|x| x.iter().map(Vec::len).min())
            .unwrap()
    }

    fn presses_for_joltages(&self) -> usize {
        fn min_presses(
            target: &[u16],
            patterns: &HashMap<Button, Vec<Vec<Button>>>,
            cache: &mut HashMap<Vec<u16>, Option<usize>>,
        ) -> Option<usize> {
            if target.iter().all(|&j| j == 0) {
                return Some(0);
            }

            if let Some(&result) = cache.get(target) {
                return result;
            }

            let mut result: Option<usize> = None;
            if let Some(combinations) = patterns.get(
                &target
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &j)| if j % 2 == 1 { Some(i) } else { None })
                    .collect(),
            ) {
                for presses in combinations {
                    let mut next_target = target.to_vec();
                    for button in presses {
                        for &idx in button {
                            next_target[idx] = next_target[idx].saturating_sub(1);
                        }
                    }

                    let halved: Vec<u16> = next_target.iter().map(|&j| j / 2).collect();
                    if let Some(half_presses) = min_presses(&halved, patterns, cache) {
                        let total_presses = presses.len() + 2 * half_presses;
                        result = Some(
                            result.map_or(total_presses, |current| current.min(total_presses)),
                        );
                    }
                }
            }
            cache.insert(target.to_vec(), result);
            result
        }

        min_presses(&self.desired_joltages, &self.patterns, &mut HashMap::new()).unwrap()
    }
}

impl<'a> From<&'a str> for Machine {
    fn from(line: &'a str) -> Self {
        let parts: Vec<&str> = line.split_whitespace().collect();

        let desired_lights = parts[0]
            .trim_matches(&['[', ']'][..])
            .chars()
            .map(|c| c == '#')
            .collect();

        let desired_joltages = parts
            .iter()
            .find(|p| p.starts_with('{') && p.ends_with('}'))
            .unwrap()
            .trim_matches(&['{', '}'][..])
            .split(',')
            .map(|s| s.parse::<u16>().unwrap())
            .collect();

        let buttons = parts[1..]
            .iter()
            .filter(|part| part.starts_with('(') && part.ends_with(')'))
            .map(|part| {
                part.trim_matches(&['(', ')'][..])
                    .split(',')
                    .filter_map(|s| s.parse::<usize>().ok())
                    .collect::<Button>()
            })
            .collect::<Vec<Button>>();

        Self::new(desired_lights, desired_joltages, &buttons)
    }
}

fn main() {
    let machines = std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(Machine::from)
        .collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        machines
            .iter()
            .map(Machine::presses_for_lights)
            .sum::<usize>()
    );
    println!(
        "The answer to the second part is: {}",
        machines
            .iter()
            .map(Machine::presses_for_joltages)
            .sum::<usize>()
    );
}
