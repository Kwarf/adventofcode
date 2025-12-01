use std::fs::read_to_string;

struct Rotation(i16);
struct State(u8);

impl State {
    fn turn(self, rotation: Rotation) -> State {
        State((self.0 as i16 + rotation.0).rem_euclid(100) as u8)
    }
}

fn parse_line(input: &str) -> Rotation {
    let (dir, value) = input.split_at(1);
    let value: i16 = value.parse().unwrap();
    match dir {
        "L" => Rotation(-value),
        "R" => Rotation(value),
        _ => unreachable!(),
    }
}

fn main() {
    println!(
        "The answer to the first part is: {}",
        read_to_string("input.txt")
            .unwrap()
            .lines()
            .fold((State(50), 0u16), |(state, zeros), line| {
                let state = state.turn(parse_line(line));
                let zeros = if state.0 == 0 { zeros + 1 } else { zeros };
                (state, zeros)
            })
            .1
    );
}
