use itertools::Itertools;

struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn area(&self, other: &Point) -> i64 {
        ((other.x - self.x).abs() + 1) * ((other.y - self.y).abs() + 1)
    }
}

fn main() {
    let points: Vec<_> = std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(|line| {
            let (x, y) = line
                .trim()
                .split(',')
                .map(|s| s.parse().unwrap())
                .collect_tuple()
                .unwrap();
            Point { x, y }
        })
        .collect();

    println!(
        "The answer to the first part is: {}",
        points
            .iter()
            .combinations(2)
            .map(|pair| pair[0].area(pair[1]))
            .max()
            .unwrap()
    );
}
