use std::{collections::HashSet, fs, str::FromStr};

#[derive(Clone, PartialEq)]
enum OpCode {
    Acc(i32),
    Jmp(i32),
    Nop(i32),
}

impl FromStr for OpCode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s[4..].parse().unwrap_or(0);
        match &s[..3] {
            "acc" => Ok(OpCode::Acc(value)),
            "jmp" => Ok(OpCode::Jmp(value)),
            "nop" => Ok(OpCode::Nop(value)),
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
            OpCode::Nop(_) => State {
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

struct BruteForcer<'a> {
    step: usize,
    original: &'a [OpCode],
}

impl Iterator for BruteForcer<'_> {
    type Item = Vec<OpCode>;

    fn next(&mut self) -> Option<Self::Item> {
        self.step = self.original.iter().enumerate().position(|(i, x)| {
            i > self.step
                && match x {
                    OpCode::Jmp(_) | OpCode::Nop(_) => true,
                    _ => false,
                }
        })?;

        let mut variant = self.original.to_vec();
        variant[self.step] = match variant[self.step] {
            OpCode::Nop(x) => OpCode::Jmp(x),
            OpCode::Jmp(x) => OpCode::Nop(x),
            _ => panic!("Matched opcode not nop or jmp"),
        };
        Some(variant)
    }
}

impl BruteForcer<'_> {
    fn new<'a>(original: &'a [OpCode]) -> BruteForcer<'a> {
        BruteForcer { step: 0, original }
    }
}

fn run_p1(state: State, program: &[OpCode]) -> i32 {
    match program[state.pc].execute(state) {
        State { pc, acc, history } if history.contains(&pc) => acc,
        new_state => run_p1(new_state, program),
    }
}

fn run_p2(state: State, program: &[OpCode]) -> Option<i32> {
    match program[state.pc].execute(state) {
        State { pc, acc, .. } if pc >= program.len() => Some(acc),
        State { pc, acc: _, history } if history.contains(&pc) => None,
        new_state => run_p2(new_state, program),
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
        run_p1(State::default(), &program)
    );

    println!(
        "The answer to the second part is: {}",
        BruteForcer::new(&program)
            .filter_map(|x| run_p2(State::default(), &x))
            .next()
            .expect("No solution to part 2 found")
    );
}
