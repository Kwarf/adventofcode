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

#[derive(PartialEq)]
enum Validation {
    Undetermined(HashSet<u8>),
    Invalid(u8),
    Valid,
}

fn validate(rules: &HashMap<u8, Vec<u8>>, printout: &[u8]) -> Validation {
    match printout.iter().fold(
        Validation::Undetermined(HashSet::from([printout[0]])),
        |validation, n| match validation {
            Validation::Undetermined(mut set) => {
                if let Some(rules) = rules.get(n) {
                    if rules.iter().any(|x| set.contains(x)) {
                        Validation::Invalid(*n)
                    } else {
                        set.insert(*n);
                        Validation::Undetermined(set)
                    }
                } else {
                    set.insert(*n);
                    Validation::Undetermined(set)
                }
            }
            _ => validation,
        },
    ) {
        Validation::Undetermined(_) => Validation::Valid,
        validation => validation,
    }
}

fn reorder(rules: &HashMap<u8, Vec<u8>>, printout: &[u8]) -> Vec<u8> {
    let mut printout = printout.to_vec();
    while let Validation::Invalid(page) = validate(rules, &printout) {
        let index = printout.iter().position(|x| x == &page).unwrap();
        printout.swap(index, index - 1);
    }
    printout
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
            .filter(|x| validate(&rules, x) == Validation::Valid)
            .map(|x| x[x.len() / 2] as u32)
            .sum::<u32>()
    );

    println!(
        "The answer to the second part is: {}",
        printouts
            .iter()
            .filter(|x| validate(&rules, x) != Validation::Valid)
            .map(|x| reorder(&rules, x))
            .map(|x| x[x.len() / 2] as u32)
            .sum::<u32>()
    );
}
