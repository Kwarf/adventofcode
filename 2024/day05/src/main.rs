use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
};

fn parse_rules(lines: &[&str]) -> HashMap<u8, Vec<u8>> {
    lines
        .iter()
        .map(|line| {
            let mut parts = line.split("|");
            (
                parts.next().unwrap().parse().unwrap(),
                parts.next().unwrap().parse().unwrap(),
            )
        })
        .fold(HashMap::new(), |mut map, (key, value)| {
            map.entry(key).or_insert_with(Vec::new).push(value);
            map
        })
}

fn parse_printouts(lines: &[&str]) -> Vec<Vec<u8>> {
    lines
        .iter()
        .map(|line| line.split(",").map(|n| n.parse().unwrap()).collect())
        .collect()
}

fn is_valid(rules: &HashMap<u8, Vec<u8>>, printout: &[u8]) -> bool {
    printout
        .iter()
        .fold(HashSet::from([printout[0]]), |mut set, n| {
            if set.is_empty() {
                set
            } else if let Some(rules) = rules.get(n) {
                if rules.iter().any(|x| set.contains(x)) {
                    HashSet::new()
                } else {
                    set.insert(*n);
                    set
                }
            } else {
                set.insert(*n);
                set
            }
        })
        .len()
        > 0
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let lines = input.lines().collect::<Vec<_>>();
    let split = lines.iter().position(|line| line.is_empty()).unwrap();
    let rules = parse_rules(&lines[..split]);
    let printouts = parse_printouts(&lines[split + 1..]);

    println!(
        "The answer to the first part is: {}",
        printouts
            .iter()
            .filter(|x| is_valid(&rules, x))
            .map(|x| x[x.len() / 2] as u32)
            .sum::<u32>()
    );
}
