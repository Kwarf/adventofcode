use std::{collections::HashMap, fs, str::FromStr};

struct PassportInfo {
    fields: HashMap<String, String>,
}

impl FromStr for PassportInfo {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(PassportInfo {
            fields: s
                .split(|x| ['\n', ' '].contains(&x))
                .filter_map(|x| {
                    let mut split = x.split(':');
                    Some((split.next()?.to_owned(), split.next()?.to_owned()))
                })
                .collect(),
        })
    }
}

fn is_valid(passport: &PassportInfo) -> bool {
    const REQUIRED_FIELDS: &'static [&'static str] =
        &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    REQUIRED_FIELDS
        .iter()
        .all(|x| passport.fields.contains_key(*x))
}

fn main() {
    let cnt = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .split("\n\n")
        .filter_map(|x| x.parse().ok())
        .filter(is_valid)
        .count();

    println!("The answer to the first part is: {}", cnt);
}
