use std::collections::HashMap;

#[derive(Clone)]
struct Diagram {
    data: Vec<char>,
    width: usize,
}

impl Diagram {
    fn get(&self, x: usize, y: usize) -> char {
        *self.data.get(y * self.width + x).unwrap()
    }

    fn set(&mut self, x: usize, y: usize, value: char) {
        self.data[y * self.width + x] = value;
    }

    fn height(&self) -> usize {
        self.data.len() / self.width
    }

    fn simulate(&mut self, position: (usize, usize)) {
        if position.0 >= self.width || position.1 == self.height() {
            return;
        }

        match self.get(position.0, position.1) {
            'S' | '.' => {
                self.set(position.0, position.1, '|');
                self.simulate((position.0, position.1 + 1));
            }
            '^' => {
                self.set(position.0 - 1, position.1, '|');
                self.set(position.0 + 1, position.1, '|');
                self.simulate((position.0 - 1, position.1 + 1));
                self.simulate((position.0 + 1, position.1 + 1));
            }
            '|' => {}
            _ => unreachable!(),
        }
    }

    fn quantum_timelines(
        &self,
        visited: &mut HashMap<(usize, usize), usize>,
        position: (usize, usize),
    ) -> usize {
        if let Some(&count) = visited.get(&position) {
            return count;
        }

        if position.1 == self.height() {
            return 1;
        }

        let result = match self.get(position.0, position.1) {
            'S' | '.' | '|' => self.quantum_timelines(visited, (position.0, position.1 + 1)),
            '^' => [position.0 - 1, position.0 + 1]
                .into_iter()
                .filter(|&x| x < self.width)
                .map(|x| self.quantum_timelines(visited, (x, position.1 + 1)))
                .sum(),
            _ => unreachable!(),
        };

        visited.insert(position, result);
        result
    }
}

fn parse(input: &str) -> Diagram {
    Diagram {
        data: input.chars().filter(|c| *c != '\n').collect(),
        width: input.lines().next().unwrap().len(),
    }
}

fn main() {
    let diagram = parse(&std::fs::read_to_string("input.txt").unwrap());

    println!("The answer to the first part is: {}", {
        let mut diagram = diagram.clone();
        diagram.simulate((diagram.data.iter().position(|c| *c == 'S').unwrap(), 0usize));
        (0..diagram.height())
            .map(|y| {
                (0..diagram.width)
                    .filter(|x| diagram.get(*x, y) == '^' && diagram.get(*x, y - 1) == '|')
                    .count()
            })
            .sum::<usize>()
    });

    println!(
        "The answer to the second part is: {}",
        diagram.quantum_timelines(
            &mut HashMap::new(),
            (diagram.data.iter().position(|c| *c == 'S').unwrap(), 0usize)
        )
    );
}
