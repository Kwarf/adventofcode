all: partone parttwo

partone:
	g++ -std=c++11 -march=native -O3 -c part1.cpp
	g++ -std=c++11 -march=native -o part1 part1.o

parttwo:
	g++ -std=c++11 -march=native -O3 -c part2.cpp
	g++ -std=c++11 -march=native -o part2 part2.o

clean:
	rm part1 part1.o
	rm part2 part2.o

run: all
	./part1 input.txt
	./part2 input.txt
