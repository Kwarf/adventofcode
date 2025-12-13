fn main() {
    println!(
        "The answer to the first part is: {}",
        std::fs::read_to_string("input.txt")
            .unwrap()
            .lines()
            .filter(|line| line.contains("x"))
            .map(|line| {
                let parts: Vec<&str> = line.split(':').collect();
                (
                    parts[0]
                        .split('x')
                        .map(|x| x.trim().parse::<usize>().unwrap())
                        .take(2)
                        .product::<usize>(),
                    parts[1]
                        .split_whitespace()
                        .map(|x| x.parse::<usize>().unwrap())
                        .sum::<usize>(),
                )
            })
            .filter(|(area, n_parts)| *area >= n_parts * 9)
            .count()
    );

    println!("The answer to the second part is to go back and finish part 2 of day 10. :(");
}
