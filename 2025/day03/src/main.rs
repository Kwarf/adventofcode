fn joltage(battery: &[char]) -> u32 {
    battery
        .iter()
        .enumerate()
        .flat_map(|(i, &first)| {
            battery
                .iter()
                .skip(i + 1)
                .map(move |&second| first.to_digit(10).unwrap() * 10 + second.to_digit(10).unwrap())
        })
        .max()
        .unwrap()
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let batteries = input.lines().collect::<Vec<&str>>();

    println!(
        "The answer to the first part is: {}",
        batteries
            .iter()
            .map(|b| joltage(&b.chars().collect::<Vec<_>>()))
            .sum::<u32>()
    );
}
