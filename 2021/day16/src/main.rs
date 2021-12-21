use std::fs::read_to_string;

#[derive(Debug)]
enum Payload {
    Operator(Vec<Packet>),
    Literal(u64),
}

#[derive(Debug)]
struct Packet {
    version: u8,
    type_id: u8,
    data: Payload,
}

fn hex_to_bin(hex: &str) -> String {
    hex.chars()
        .map(|x| format!("{:04b}", usize::from_str_radix(&x.to_string(), 16).unwrap()))
        .collect()
}

fn bin_to_u8(bin: &str) -> u8 {
    u8::from_str_radix(bin, 2).unwrap()
}

fn bin_to_u64(bin: &str) -> u64 {
    u64::from_str_radix(bin, 2).unwrap()
}

fn read_literal(data: &mut dyn Iterator<Item = char>) -> Payload {
    let mut b = String::new();
    while data.next().unwrap() == '1' {
        b.extend(data.take(4));
    }
    b.extend(data.take(4));
    Payload::Literal(bin_to_u64(&b))
}

fn read_subpackets(data: &mut dyn Iterator<Item = char>) -> Payload {
    let length_bits = match data.next().unwrap() {
        '0' => 15, // length is a 15-bit number representing the number of bits in the sub-packets
        _ => 11,   // length is a 11-bit number representing the number of sub-packets
    };

    let length = bin_to_u64(&data.take(length_bits).collect::<String>());
    Payload::Operator(match length_bits {
        15 => {
            let mut subpacket_data = data.take(length as usize).peekable();
            let mut subpackets = Vec::new();
            while subpacket_data.peek() != None {
                subpackets.push(parse(&mut subpacket_data));
            }
            subpackets
        }
        _ => (0..length).map(|_| parse(data)).collect(),
    })
}

fn parse(data: &mut dyn Iterator<Item = char>) -> Packet {
    let version = bin_to_u8(&data.take(3).collect::<String>());
    let type_id = bin_to_u8(&data.take(3).collect::<String>());

    match type_id {
        4 => Packet {
            version,
            type_id,
            data: read_literal(data),
        },
        _ => Packet {
            version,
            type_id,
            data: read_subpackets(data),
        },
    }
}

fn sum_version(packet: &Packet) -> u32 {
    (packet.version as u32)
        + match &packet.data {
            Payload::Operator(packets) => packets.iter().map(sum_version).sum(),
            _ => 0,
        }
}

fn decode(packet: &Packet) -> u64 {
    match &packet.data {
        Payload::Literal(n) => *n,
        Payload::Operator(packets) => match packet.type_id {
            0 => packets.iter().map(decode).sum(),
            1 => packets.iter().map(decode).fold(1, |acc, x| acc * x),
            2 => packets.iter().map(decode).min().unwrap(),
            3 => packets.iter().map(decode).max().unwrap(),
            5 => if decode(&packets[0]) > decode(&packets[1]) { 1 } else { 0 },
            6 => if decode(&packets[0]) < decode(&packets[1]) { 1 } else { 0 },
            7 => if decode(&packets[0]) == decode(&packets[1]) { 1 } else { 0 },
            _ => panic!("Unknown packet type {}", packet.type_id),
        }
    }
}

fn main() {
    let input = parse(&mut hex_to_bin(&read_to_string("input.txt").unwrap()).chars());

    println!("The answer to the first part is: {}", sum_version(&input));
    println!("The answer to the second part is: {}", decode(&input));
}
