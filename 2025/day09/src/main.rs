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

    let candidates: Vec<_> = points
        .iter()
        .tuple_combinations()
        .sorted_by_key(|(p1, p2)| -p1.area(p2))
        .collect();

    println!(
        "The answer to the first part is: {}",
        candidates[0].0.area(candidates[0].1)
    );

    println!(
        "The answer to the second part is: {}",
        candidates
            .iter()
            .find_map(|(p1, p2)| {
                let (x_min, x_max, y_min, y_max) = (
                    p1.x.min(p2.x),
                    p1.x.max(p2.x),
                    p1.y.min(p2.y),
                    p1.y.max(p2.y),
                );

                let n = points.len();
                for i in 0..n {
                    let a = &points[i];
                    let b = &points[(i + 1) % n];

                    if (a.x == b.x
                        && a.x > x_min
                        && a.x < x_max
                        && a.y.max(b.y) > y_min
                        && a.y.min(b.y) < y_max)
                        || (a.y == b.y
                            && a.y > y_min
                            && a.y < y_max
                            && a.x.max(b.x) > x_min
                            && a.x.min(b.x) < x_max)
                    {
                        return None;
                    }
                }
                Some(p1.area(p2))
            })
            .unwrap()
    );
}
