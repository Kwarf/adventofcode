use range_set_blaze::RangeSetBlaze;

fn parse_input(input: &str) -> (RangeSetBlaze<usize>, Vec<usize>) {
    let mut input_parts = input.split("\n\n");
    let ranges = input_parts.next().unwrap();
    let available_ids = input_parts.next().unwrap();
    (
        ranges
            .lines()
            .map(|line| {
                let (start, end) = line.split_once('-').unwrap();
                start.parse().unwrap()..=end.parse().unwrap()
            })
            .collect(),
        available_ids
            .lines()
            .map(|line| line.parse().unwrap())
            .collect(),
    )
}

fn main() {
    let (database, ids) = parse_input(&std::fs::read_to_string("input.txt").unwrap());

    println!(
        "The answer to the first part is: {}",
        ids.iter().filter(|&&id| database.contains(id)).count()
    );

    println!("The answer to the second part is: {}", database.len());
}
