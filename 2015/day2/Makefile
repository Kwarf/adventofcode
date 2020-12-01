all:
	gcc -march=native -S -c main.c
	gcc -march=native -O3 -pipe -c main.c
	gcc -march=native -o main main.o

clean:
	rm main main.o main.s

run: all
	./main input.txt