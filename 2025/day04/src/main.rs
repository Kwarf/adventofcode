#[derive(Default)]
struct Map {
    width: usize,
    data: Vec<char>,
}

impl Map {
    fn has_roll(&self, x: usize, y: usize) -> bool {
        self.data[y * self.width + x] == '@'
    }

    fn accessible_rolls(&self) -> Vec<(usize, usize)> {
        #[rustfmt::skip]
        let directions = [
            (-1, -1), (0, -1), (1, -1),
            (-1,  0),          (1,  0),
            (-1,  1), (0,  1), (1,  1),
        ];

        let height = self.data.len() / self.width;
        (0..height)
            .flat_map(|y| {
                (0..self.width).filter_map(move |x| {
                    if !self.has_roll(x, y) {
                        return None;
                    }

                    if directions
                        .iter()
                        .filter(|(dx, dy)| {
                            let nx = x as isize + dx;
                            let ny = y as isize + dy;
                            nx >= 0
                                && ny >= 0
                                && nx < self.width as isize
                                && ny < height as isize
                                && self.has_roll(nx as usize, ny as usize)
                        })
                        .count()
                        < 4
                    {
                        Some((x, y))
                    } else {
                        None
                    }
                })
            })
            .collect()
    }

    fn remove(&mut self, positions: &[(usize, usize)]) {
        for (x, y) in positions {
            self.data[y * self.width + x] = '.';
        }
    }
}

fn main() {
    let mut map = std::fs::read_to_string("input.txt").unwrap().lines().fold(
        Map::default(),
        |mut map, line| {
            if map.width == 0 {
                map.width = line.len();
            }
            map.data.extend(line.chars());
            map
        },
    );

    println!(
        "The answer to the first part is: {}",
        map.accessible_rolls().len()
    );

    println!("The answer to the second part is: {}", {
        let mut removable = 0;
        loop {
            let positions = map.accessible_rolls();
            let n = positions.len();
            if n == 0 {
                break removable;
            }
            removable += n;
            map.remove(&positions);
        }
    });
}
