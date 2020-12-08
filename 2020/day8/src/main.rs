use std::{collections::HashSet, fs, str::FromStr};

enum OpCode {
    Acc(i32),
    Jmp(i32),
    Nop,
}

impl FromStr for OpCode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s[4..].parse().unwrap_or(0);
        match &s[..3] {
            "acc" => Ok(OpCode::Acc(value)),
            "jmp" => Ok(OpCode::Jmp(value)),
            "nop" => Ok(OpCode::Nop),
            _ => panic!("Unhandled opcode"),
        }
    }
}

impl OpCode {
    fn execute(&self, state: State) -> State {
        let mut history = state.history;
        history.insert(state.pc);

        match self {
            OpCode::Acc(x) => State {
                pc: state.pc + 1,
                acc: state.acc + x,
                history,
            },
            OpCode::Jmp(x) => State {
                pc: (state.pc as i32 + x) as usize,
                acc: state.acc,
                history,
            },
            OpCode::Nop => State {
                pc: state.pc + 1,
                acc: state.acc,
                history,
            },
        }
    }
}

#[derive(Default)]
struct State {
    pc: usize,
    acc: i32,
    history: HashSet<usize>,
}

fn run(state: State, program: &[OpCode]) -> i32 {
    match program[state.pc].execute(state) {
        State { pc, acc, history } if history.contains(&pc) => acc,
        new_state => run(new_state, program),
    }
}

fn main() {
    let program = fs::read_to_string("input.txt")
        .expect("Input file not found")
        .lines()
        .filter_map(|x| x.parse().ok())
        .collect::<Vec<OpCode>>();

    println!(
        "The answer to the first part is: {}",
        run(State::default(), &program)
    );
}
