#include <atomic>
#include <iomanip>
#include <iostream>
#include <regex>
#include <sstream>
#include <thread>
#include <vector>

#include <openssl/md5.h>

void advent_coin_miner(std::atomic_uint& result, int start, const int step, const std::string& key, const int zeros)
{
	std::ostringstream regexString;
	regexString << "^0{" << zeros << ",}";
	std::regex coinRegex(regexString.str());

	unsigned char md5Data[MD5_DIGEST_LENGTH];
	for (int i = start; result == 0; i += step)
	{
		std::ostringstream testString;
		testString << key << i;
		MD5(
			reinterpret_cast<const unsigned char*>(testString.str().data()),
			testString.str().length(),
			md5Data
		);

		std::ostringstream md5String;
		for (int c = 0; c < MD5_DIGEST_LENGTH; c++)
		{
			md5String
				<< std::setw(2)
				<< std::setfill('0')
				<< std::hex
				<< static_cast<int>(md5Data[c]);
		}

		if (std::regex_search(md5String.str(), coinRegex))
		{
			result = i;
		}
	}
	return;
}

int main(int argc, char* argv[])
{
	if (argc != 3)
	{
		std::cout << "This program requires two command line arguments, \
			the number of zeros to match and the secret key." << std::endl;
		return -1;
	}
	const int zeros = std::stoi(argv[1]);
	const std::string key(argv[2]);
	const unsigned int threadCount = std::thread::hardware_concurrency();
	std::cout << "Spawning " << threadCount << " worker threads..." << std::endl;

	std::atomic_uint result(0);
	std::vector<std::thread> threads;
	for (int i = 0; i < threadCount; i++)
	{
		threads.emplace_back(
			advent_coin_miner,
			std::ref(result),
			i,
			threadCount,
			std::ref(key),
			zeros
		);
	}

	for (auto& thread : threads)
	{
		thread.join();
	}

	std::cout << "The AdventCoin key is " << result << std::endl;
	return 0;
}
