all:
	g++ -std=c++11 -march=native -O3 -c main.cpp
	g++ -std=c++11 -march=native -o main main.o -lpthread -lcrypto

clean:
	rm main main.o

run: all
	./main 5 ckczppom
	./main 6 ckczppom
