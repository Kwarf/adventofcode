struct Problem<'a> {
    numbers: Vec<&'a str>,
    range: std::ops::Range<usize>,
    operator: char,
}

impl Problem<'_> {
    fn vertical_numbers(&self) -> Vec<u64> {
        let mut numbers = vec![0; self.range.end - self.range.start];
        for (x, n) in numbers.iter_mut().enumerate() {
            for c in self
                .numbers
                .iter()
                .map(|s| s.chars().nth(x).unwrap())
                .filter(|c| *c != ' ')
            {
                *n = *n * 10 + u64::from(c.to_digit(10).unwrap());
            }
        }
        numbers
    }
}

fn parse<'a>(mut input: impl DoubleEndedIterator<Item = &'a str>) -> Vec<Problem<'a>> {
    let mut problems = Vec::new();
    let operators = input.next_back().unwrap();
    let mut w = 1;
    for n in (0..operators.len()).rev() {
        match operators.chars().nth(n).unwrap() {
            operator @ ('+' | '*') => {
                problems.push(Problem {
                    numbers: Vec::new(),
                    range: n..n + w,
                    operator,
                });
                w = 0;
            }
            ' ' => w += 1,
            _ => unreachable!(),
        }
    }

    for line in input {
        for problem in &mut problems {
            problem.numbers.push(&line[problem.range.clone()]);
        }
    }

    problems
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let problems = parse(input.lines());

    println!(
        "The answer to the first part is: {}",
        problems
            .iter()
            .map(|x| (
                x.operator,
                x.numbers.iter().map(|n| n.trim().parse::<u64>().unwrap())
            ))
            .map(|(op, ns)| match op {
                '+' => ns.sum::<u64>(),
                '*' => ns.product::<u64>(),
                _ => unreachable!(),
            })
            .sum::<u64>()
    );

    println!(
        "The answer to the second part is: {}",
        problems
            .iter()
            .map(|x| (x.operator, x.vertical_numbers().into_iter()))
            .map(|(op, ns)| match op {
                '+' => ns.sum::<u64>(),
                '*' => ns.product::<u64>(),
                _ => unreachable!(),
            })
            .sum::<u64>()
    );
}
