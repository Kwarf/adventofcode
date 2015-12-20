use std::collections::HashSet;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn get_new_position(old_position: &(i32, i32), direction: char) -> (i32, i32)
{
	let &(x, y) = old_position;
	match direction
	{
		'^' => (x    , y + 1),
		'>' => (x + 1, y    ),
		'v' => (x    , y - 1),
		'<' => (x - 1, y    ),
		_ => { panic!("Invalid input character: {}", direction); }
	}
}

fn main()
{
	if env::args().count() != 2
	{
		println!("This program requires one command line argument, the filename of your puzzle input data file.");
	}
	
	let filename = env::args().last().unwrap();
	let path = Path::new(&filename);
	let mut file = match File::open(&path)
	{
		Err(why) => panic!("Failed to open input data file: {}", Error::description(&why)),
		Ok(file) => file,
	};

	let mut input_directions = String::new();
	match file.read_to_string(&mut input_directions)
	{
		Err(why) => panic!("Failed to read data file: {}", Error::description(&why)),
		Ok(_) => {},
	}

	// Part One - Single Santa
	let mut single_santa_locations = HashSet::new();
	let mut santa_position = (0, 0);
	single_santa_locations.insert(santa_position);
	for c in input_directions.chars()
	{
		santa_position = get_new_position(&santa_position, c);
		single_santa_locations.insert(santa_position);
	}
	println!("Santa visited {} houses", single_santa_locations.len());
	
	// Part Two - Santa and Robo-Santa
	let mut dual_santa_locations = HashSet::new();
	let mut santa_position = (0, 0);
	let mut robo_santa_position = (0, 0);
	single_santa_locations.insert(santa_position);
	for (i, c) in input_directions.chars().enumerate()
	{
		if i % 2 == 0
		{
			santa_position = get_new_position(&santa_position, c);
			dual_santa_locations.insert(santa_position);
		}
		else
		{
			robo_santa_position = get_new_position(&robo_santa_position, c);
			dual_santa_locations.insert(robo_santa_position);
		}
	}
	println!("Santa and Robo-Santa visited {} houses", dual_santa_locations.len());
}
