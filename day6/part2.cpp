#include <array>
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

	std::array<unsigned int, 1000 * 1000> lights;
	lights.fill(0);
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
					const auto arrayIndex = x + y * 1000;
					switch (action)
					{
						case Action::TurnOn:
							lights[arrayIndex]++;
							break;
						case Action::TurnOff:
							if (lights[arrayIndex] > 0)
								lights[arrayIndex]--;
							break;
						case Action::Toggle:
							lights[arrayIndex] += 2;
							break;
					}
				}
			}
		}
	}


	unsigned int brightness = 0;
	for (const auto& light : lights)
	{
		brightness += light;
	}
	std::cout << "Total brightness: " << brightness << std::endl;

	return 0;
}
