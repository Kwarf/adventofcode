struct Problem {
    numbers: Vec<u64>,
    operator: char,
}

fn parse<'a>(mut input: impl DoubleEndedIterator<Item = &'a str>) -> Vec<Problem> {
    let mut problems = input
        .next_back()
        .unwrap()
        .split_whitespace()
        .map(|x| Problem {
            numbers: Vec::new(),
            operator: x.chars().next().unwrap(),
        })
        .collect::<Vec<_>>();

    for line in input {
        for (i, value) in line.split_whitespace().enumerate() {
            problems[i].numbers.push(value.parse().unwrap());
        }
    }
    problems
}

fn solve(problem: &Problem) -> u64 {
    match problem.operator {
        '+' => problem.numbers.iter().sum(),
        '*' => problem.numbers.iter().product(),
        _ => unreachable!(),
    }
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let problems = parse(input.lines());

    println!(
        "The answer to the first part is: {}",
        problems.iter().map(solve).sum::<u64>()
    );
}
