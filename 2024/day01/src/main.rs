use std::{fs::read_to_string, str::FromStr};

fn parse_line<T>(line: &str) -> (T, T)
where
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Debug,
{
    let mut iter = line.split_whitespace();
    (
        iter.next().unwrap().parse().unwrap(),
        iter.next().unwrap().parse().unwrap(),
    )
}

fn main() {
    let (lefts, rights) = {
        let (mut l, mut r) = read_to_string("input.txt")
            .unwrap()
            .lines()
            .map(parse_line::<i32>)
            .unzip::<_, _, Vec<_>, Vec<_>>();
        l.sort_unstable();
        r.sort_unstable();
        (l, r)
    };

    println!(
        "The answer to the first part is: {}",
        lefts
            .iter()
            .zip(rights.iter())
            .map(|(&left, &right)| (left - right).abs())
            .sum::<i32>()
    );

    println!(
        "The answer to the second part is: {}",
        lefts
            .iter()
            .map(|&left| left as usize * rights.iter().filter(|&&right| right == left).count())
            .sum::<usize>()
    );
}
