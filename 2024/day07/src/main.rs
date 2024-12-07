use std::fs::read_to_string;

#[derive(Debug)]
struct Problem {
    expected: u64,
    values: Vec<u64>,
}

impl Problem {
    fn is_solvable(&self, allow_concatenation: bool) -> bool {
        fn solve(v: &u64, values: &[u64], expected: u64, allow_concatenation: bool) -> bool {
            if values.is_empty() {
                return *v == expected;
            }
            solve(
                &(v + values[0]),
                &values[1..],
                expected,
                allow_concatenation,
            ) || solve(
                &(v * values[0]),
                &values[1..],
                expected,
                allow_concatenation,
            ) || if allow_concatenation {
                let concatenated = format!("{}{}", v, values[0].to_string()).parse().unwrap();
                solve(&concatenated, &values[1..], expected, allow_concatenation)
            } else {
                false
            }
        }

        solve(
            &self.values[0],
            &self.values[1..],
            self.expected,
            allow_concatenation,
        )
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
            .filter(|p| p.is_solvable(false))
            .map(|x| x.expected)
            .sum::<u64>()
    );

    println!(
        "The answer to the second part is: {}",
        problems
            .iter()
            .filter(|p| p.is_solvable(true))
            .map(|x| x.expected)
            .sum::<u64>()
    );
}
