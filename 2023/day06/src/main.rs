#[derive(Clone, Copy)]
struct Race {
    time: u64,
    distance: u64,
}

impl Race {
    fn trials(self) -> impl Iterator<Item = u64> {
        1..self.time
    }

    fn is_faster(self, hold: u64) -> bool {
        hold * (self.time - hold) > self.distance
    }
}

fn main() {
    // Time:        54     70     82     75
    // Distance:   239   1142   1295   1253
    let races = [
        Race {
            time: 54,
            distance: 239,
        },
        Race {
            time: 70,
            distance: 1142,
        },
        Race {
            time: 82,
            distance: 1295,
        },
        Race {
            time: 75,
            distance: 1253,
        },
    ];

    println!(
        "The answer to the first part is: {}",
        races
            .iter()
            .map(|x| x.trials().filter(|hold| x.is_faster(*hold)).count())
            .product::<usize>()
    );

    let race = Race {
        time: 54708275,
        distance: 239114212951253,
    };

    println!(
        "The answer to the second part is: {}",
        race.trials().filter(|hold| race.is_faster(*hold)).count()
    );
}
