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
}

fn parse(input: &str) -> Diagram {
    let mut diagram = Diagram {
        data: input.chars().filter(|c| *c != '\n').collect(),
        width: input.lines().next().unwrap().len(),
    };
    diagram.simulate((diagram.data.iter().position(|c| *c == 'S').unwrap(), 0usize));
    diagram
}

fn main() {
    let diagram = parse(&std::fs::read_to_string("input.txt").unwrap());

    println!(
        "The answer to the first part is: {}",
        (0..diagram.height())
            .map(|y| {
                (0..diagram.width)
                    .filter(|x| diagram.get(*x, y) == '^' && diagram.get(*x, y - 1) == '|')
                    .count()
            })
            .sum::<usize>()
    );
}
