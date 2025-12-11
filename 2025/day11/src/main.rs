struct Device<'a> {
    name: &'a str,
    connections: Vec<&'a str>,
}

impl<'a> From<&'a str> for Device<'a> {
    fn from(s: &'a str) -> Self {
        let parts: Vec<&str> = s.split(':').collect();
        Device {
            name: parts[0].trim(),
            connections: parts[1]
                .split_whitespace()
                .map(|conn| conn.trim())
                .collect(),
        }
    }
}

fn dfs<'a>(
    devices: &'a [Device<'a>],
    current: &'a str,
    target: &'a str,
    visited: &mut Vec<&'a str>,
    count: &mut usize,
) {
    if current == target {
        *count += 1;
        return;
    }

    for next in devices
        .iter()
        .find(|d| d.name == current)
        .map(|d| &d.connections)
        .unwrap()
    {
        if !visited.contains(&next) {
            visited.push(next);
            dfs(devices, next, target, visited, count);
            visited.pop();
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let devices: Vec<_> = input.lines().map(Device::from).collect();

    println!("The answer to the first part is: {}", {
        let mut count = 0;
        let mut visited = vec!["you"];
        dfs(&devices, "you", "out", &mut visited, &mut count);
        count
    });
}
