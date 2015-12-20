#include <array>
#include <bitset>
#include <iostream>
#include <fstream>
#include <regex>

enum class Action
{
	None,
	TurnOn,
	TurnOff,
	Toggle
};

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		std::cout << "This program requires one command line argument, "
			"the filename of your puzzle input data file." << std::endl;
	}

	std::array<std::bitset<1000>, 1000> lights;
	std::ifstream input(argv[1]);
	std::string line;
	while (std::getline(input, line))
	{
		std::regex rx("(.+) (\\d+),(\\d+) through (\\d+),(\\d+)");
		std::smatch match;
		if (std::regex_match(line, match, rx))
		{
			const auto& actionString = match[1].str();
			const auto x1 = std::stoi(match[2].str());
			const auto y1 = std::stoi(match[3].str());
			const auto x2 = std::stoi(match[4].str());
			const auto y2 = std::stoi(match[5].str());

			Action action = Action::None;
			if (actionString == "turn on")
			{
				action = Action::TurnOn;
			}
			else if (actionString == "turn off")
			{
				action = Action::TurnOff;
			}
			else if (actionString == "toggle")
			{
				action = Action::Toggle;
			}

			for (int x = x1; x <= x2; x++)
			{
				for (int y = y1; y <= y2; y++)
				{
					switch (action)
					{
						case Action::TurnOn:
							lights[y].set(x);
							break;
						case Action::TurnOff:
							lights[y].reset(x);
							break;
						case Action::Toggle:
							lights[y].flip(x);
							break;
					}
				}
			}
		}
	}


	unsigned int lightsOn = 0;
	for (const auto& row : lights)
	{
		lightsOn += row.count();
	}
	std::cout << lightsOn << " lights are on." << std::endl;

	return 0;
}
