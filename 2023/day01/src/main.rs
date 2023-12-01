use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn decode(line: &str) -> u32 {
    let values = line
        .chars()
        .filter_map(|x| x.to_digit(10))
        .fold((None, None), |acc, x| (acc.0.or(Some(x)), Some(x)));

    values.0.unwrap_or_default() * 10 + values.1.unwrap_or_default()
}

fn main() -> std::io::Result<()> {
    let file = File::open("input.txt")?;

    println!(
        "The answer to the first part is: {}",
        BufReader::new(file)
            .lines()
            .map(|x| decode(&x.unwrap()))
            .sum::<u32>()
    );

    Ok(())
}
