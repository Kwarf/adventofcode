use std::{fs::read_to_string, str::FromStr};

#[derive(PartialEq)]
enum Safety {
    Unknown(i8),
    Decreasing(i8),
    Increasing(i8),
    Unsafe,
}

#[derive(Debug)]
struct Report(Vec<i8>);

impl Report {
    fn is_safe(&self) -> bool {
        fn valid_step(diff: i8) -> bool {
            diff.abs() != 0 && diff.abs() <= 3
        }

        self.0
            .iter()
            .skip(1)
            .fold(Safety::Unknown(self.0[0]), |s, &n| match s {
                Safety::Unknown(x) => {
                    let diff = n - x;
                    if !valid_step(diff) {
                        Safety::Unsafe
                    } else if diff.signum() == 1 {
                        Safety::Increasing(n)
                    } else {
                        Safety::Decreasing(n)
                    }
                }
                Safety::Decreasing(x) => {
                    let diff = n - x;
                    if !valid_step(diff) || diff.signum() == 1 {
                        Safety::Unsafe
                    } else {
                        Safety::Decreasing(n)
                    }
                }
                Safety::Increasing(x) => {
                    let diff = n - x;
                    if !valid_step(diff) || diff.signum() == -1 {
                        Safety::Unsafe
                    } else {
                        Safety::Increasing(n)
                    }
                }
                Safety::Unsafe => Safety::Unsafe,
            })
            != Safety::Unsafe
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
}
