#[derive(Copy, Clone)]
struct Rotation(i16);

#[derive(Copy, Clone)]
struct State(u8);

impl State {
    fn turn(self, rotation: Rotation) -> State {
        State((i16::from(self.0) + rotation.0).rem_euclid(100) as u8)
    }

    fn turn_stepped(self, rotation: Rotation) -> (State, u16) {
        (0..rotation.0.abs()).fold((self, 0), |(current, zeros), _| {
            let state = current.turn(Rotation(rotation.0.signum()));
            (state, zeros + u16::from(state.0 == 0))
        })
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
    let rotations = std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(parse_line)
        .collect::<Vec<_>>();

    println!(
        "The answer to the first part is: {}",
        rotations
            .iter()
            .fold((State(50), 0u16), |(state, zeros), rotation| {
                let state = state.turn(*rotation);
                (state, zeros + u16::from(state.0 == 0))
            })
            .1
    );

    println!(
        "The answer to the second part is: {}",
        rotations
            .iter()
            .fold((State(50), 0u16), |(state, zeros), rotation| {
                let (new_state, crossings) = state.turn_stepped(*rotation);
                (new_state, zeros + crossings)
            })
            .1
    );
}
