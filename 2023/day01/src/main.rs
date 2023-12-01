use std::fs::read_to_string;

fn decode(line: &str) -> u32 {
    line.chars()
        .filter_map(|x| x.to_digit(10))
        .fold([None, None], |[first, _], x| {
            [first.or(Some(x * 10)), Some(x)]
        })
        .map(|x| x.unwrap())
        .into_iter()
        .sum()
}

fn lenient_decode(line: &str) -> u32 {
    const LOOKUP: [&str; 9] = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    let to_number = |number: &str| {
        if let Some(n) = LOOKUP.iter().position(|&x| number.starts_with(x)) {
            Some(n as u32 + 1)
        } else {
            number.chars().next().and_then(|x| x.to_digit(10))
        }
    };

    (0..line.len())
        .fold([None, None], |[first, last], n| {
            [
                first.or(to_number(&line[n..]).map(|x| x * 10)),
                last.or(to_number(&line[line.len() - 1 - n..])),
            ]
        })
        .map(|x| x.unwrap())
        .into_iter()
        .sum()
}

fn main() {
    let file = read_to_string("input.txt").unwrap();

    println!(
        "The answer to the first part is: {}",
        file.lines().map(decode).sum::<u32>()
    );

    println!(
        "The answer to the second part is: {}",
        file.lines().map(lenient_decode).sum::<u32>()
    );
}
