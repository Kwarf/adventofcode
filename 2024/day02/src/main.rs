use std::{fs::read_to_string, str::FromStr};

use itertools::Itertools;

#[derive(Debug)]
struct Report(Vec<i8>);

impl Report {
    fn is_safe(&self) -> bool {
        let (diffs, signs) = self
            .0
            .iter()
            .tuple_windows()
            .map(|(a, b)| ((a - b).abs(), (a - b).signum()))
            .unzip::<_, _, Vec<_>, Vec<_>>();

        diffs.iter().all(|&diff| diff != 0 && diff <= 3) && (signs.iter().dedup().count() == 1)
    }

    fn dampened_reports<'a>(&'a self) -> impl Iterator<Item = Report> + 'a {
        (0..self.0.len()).map(|i| {
            let mut modified = self.0.clone();
            modified.remove(i);
            Report(modified)
        })
    }
}

impl FromStr for Report {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Report(
            s.split_ascii_whitespace()
                .map(|x| x.parse().unwrap())
                .collect(),
        ))
    }
}

fn main() {
    let reports = read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(|x| x.parse().unwrap())
        .collect::<Vec<Report>>();

    println!(
        "The answer to the first part is: {}",
        reports.iter().filter(|x| x.is_safe()).count()
    );

    println!(
        "The answer to the second part is: {}",
        reports
            .iter()
            .filter(|x| x.is_safe() || x.dampened_reports().any(|x| x.is_safe()))
            .count()
    );
}
