fn joltage(cells: usize, battery: &[char]) -> u64 {
    let mut stack = Vec::with_capacity(cells);
    for (i, &digit) in battery.iter().enumerate() {
        while let Some(&last) = stack.last() {
            if digit <= last || stack.len() + battery.len() - i - 1 < cells {
                break;
            }
            stack.pop();
        }

        if stack.len() < cells {
            stack.push(digit);
        }
    }
    stack.into_iter().collect::<String>().parse().unwrap()
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let batteries = input.lines().collect::<Vec<&str>>();

    println!(
        "The answer to the first part is: {}",
        batteries
            .iter()
            .map(|b| joltage(2, &b.chars().collect::<Vec<_>>()))
            .sum::<u64>()
    );

    println!(
        "The answer to the second part is: {}",
        batteries
            .iter()
            .map(|b| joltage(12, &b.chars().collect::<Vec<_>>()))
            .sum::<u64>()
    );
}
