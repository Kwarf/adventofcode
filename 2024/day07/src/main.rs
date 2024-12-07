use std::fs::read_to_string;

#[derive(Debug)]
struct Problem {
    expected: u64,
    values: Vec<u64>,
}

impl Problem {
    fn is_solvable(&self) -> bool {
        fn solve(v: &u64, values: &[u64], expected: u64) -> bool {
            if values.is_empty() {
                return *v == expected;
            }
            solve(&(v + values[0]), &values[1..], expected)
                || solve(&(v * values[0]), &values[1..], expected)
        }

        solve(&self.values[0], &self.values[1..], self.expected)
    }
}

fn parse_problem(line: &str) -> Problem {
    let mut parts = line.split(": ");
    let expected = parts.next().unwrap().parse().unwrap();
    let values = parts
        .next()
        .unwrap()
        .split(" ")
        .map(|x| x.parse().unwrap())
        .collect();
    Problem { expected, values }
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let problems = input.lines().map(parse_problem).collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        problems
            .iter()
            .filter(|p| p.is_solvable())
            .map(|x| x.expected)
            .sum::<u64>()
    );
}
