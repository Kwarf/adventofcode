#include <stdio.h>
#include <stdlib.h>

#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

typedef int v4si __attribute__((vector_size(16)));

union i4vec
{
	v4si v;
	int i[4];
};

int get_smallest_value(union i4vec vec)
{
	return vec.i[0] < vec.i[1] ?
	       (vec.i[0] < vec.i[2] ? vec.i[0] : vec.i[2]) :
	       (vec.i[1] < vec.i[2] ? vec.i[1] : vec.i[2]);
}

int get_shortest_distance_around(union i4vec vec)
{
	if (vec.i[0] >= vec.i[1] && vec.i[0] >= vec.i[2])
	{
		return vec.i[1] * 2 + vec.i[2] * 2;
	}
	else if (vec.i[1] >= vec.i[0] && vec.i[1] >= vec.i[2])
	{
		return vec.i[0] * 2 + vec.i[2] * 2;
	}
	else
	{
		return vec.i[0] * 2 + vec.i[1] * 2;
	}
}

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		puts("This program requires one command line argument, the filename of your puzzle input data file.");
		return 0;
	}

	int fd = open(argv[1], O_RDONLY);
	if (fd < 0)
	{
		puts("Failed to open input data file.");
		return -1;
	}

	struct stat file_stat;
	int result = fstat(fd, &file_stat);
	if (result < 0)
	{
		puts("Failed to open input data file.");
		return -1;
	}

	void* const data = mmap(NULL, file_stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (data == MAP_FAILED)
	{
		puts("Failed to mmap data file.");
		return -1;
	}

	char* data_ptr = data;
	int required_wrapping_paper = 0;
	int required_ribbon = 0;
	while (*data_ptr != '\0')
	{
		union i4vec vector = {.v = {0, 0, 0, 0}};
		for (int i = 0; i < 3; i++)
		{
			char numberString[8]; // Assume that the length of one side is never higher than 7 numbers
			memset(&numberString, 0, sizeof(numberString)); // Zero out the string to ensure null termination
			for (int c = 0; *data_ptr != 'x' && *data_ptr != '\n' && c < sizeof(numberString) - 1; c++)
			{
				numberString[c] = *data_ptr++;
			}
			vector.i[i] = atoi(numberString);
			data_ptr++;
		}

		// Calculate how much ribbon is needed
		const int bow_ribbon = vector.i[0] * vector.i[1] * vector.i[2];
		const int wrap_ribbon = get_shortest_distance_around(vector);

		// vector has l, w, h
		// multiply by w, h, l
		vector.v *= __builtin_shuffle(vector.v, (v4si){1, 2, 0, 3});

		// The elves need extra paper equal to the area of the smallest side
		const int slack = get_smallest_value(vector);

		// Calculate surface area of the entire box
		vector.v *= 2;
		const int surface_area = vector.i[0] + vector.i[1] + vector.i[2];

		required_wrapping_paper += surface_area + slack;
		required_ribbon += bow_ribbon + wrap_ribbon;
	}

	printf("The elves should order %i square feet of wrapping paper and %i feet of ribbon\n",
	       required_wrapping_paper,
	       required_ribbon);

	// Cleanup
	munmap(data, file_stat.st_size);
	close(fd);

	return 0;
}