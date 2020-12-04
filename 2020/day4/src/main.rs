use std::{collections::HashMap, fs, str::FromStr};

use regex::Regex;

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

fn is_valid_p1(passport: &PassportInfo) -> bool {
    const REQUIRED_FIELDS: &'static [&'static str] =
        &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    REQUIRED_FIELDS
        .iter()
        .all(|x| passport.fields.contains_key(*x))
}

fn create_numeric_within_validator(min: i32, max: i32) -> Box<dyn Fn(&str) -> bool> {
    Box::new(move |x| {
        let value = x.parse().unwrap_or(0);
        value >= min && value <= max
    })
}

fn create_regex_validator(pattern: &str) -> Box<dyn Fn(&str) -> bool> {
    let re = Regex::new(pattern).unwrap();
    Box::new(move |x| re.is_match(x))
}

fn is_valid_p2(passport: &PassportInfo, rules: &[(&str, Box<dyn Fn(&str) -> bool>)]) -> bool {
    rules
        .iter()
        .all(|(field, validator)| match passport.fields.get(*field) {
            Some(value) => (*validator)(value),
            None => false,
        })
}

fn main() {
    let passports = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .split("\n\n")
        .filter_map(|x| x.parse().ok())
        .collect::<Vec<PassportInfo>>();

    println!(
        "The answer to the first part is: {}",
        passports.iter().filter(|x| is_valid_p1(x)).count()
    );

    // Time for that second part.. yay for lots of rules..
    let rules: [(&str, Box<dyn Fn(&str) -> bool>); 7] = [
        ("byr", create_numeric_within_validator(1920, 2002)),
        ("iyr", create_numeric_within_validator(2010, 2020)),
        ("eyr", create_numeric_within_validator(2020, 2030)),
        (
            "hgt",
            Box::new(|x| {
                let re = Regex::new(r"^(\d+)(cm|in)$").unwrap();
                match re.captures(x).and_then(|x| {
                    Some((x.get(1)?.as_str().parse::<i32>().ok()?, x.get(2)?.as_str()))
                }) {
                    Some((length, "cm")) => length >= 150 && length <= 193,
                    Some((length, "in")) => length >= 59 && length <= 76,
                    Some((_, _)) => false,
                    None => false,
                }
            }),
        ),
        ("hcl", create_regex_validator(r"^#[0-9a-f]{6}$")),
        ("ecl", create_regex_validator(r"^(?:amb|blu|brn|gry|grn|hzl|oth)$")),
        ("pid", create_regex_validator(r"^\d{9}$")),
    ];

    println!(
        "The answer to the second part is: {}",
        passports.iter().filter(|x| is_valid_p2(x, &rules)).count()
    );
}
