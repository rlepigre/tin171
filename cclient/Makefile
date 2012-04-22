all: client clean

client: client.o board.o
	gcc client.o board.o -o client

client.o: client.c
	gcc -c client.c

board.o: board.c
	gcc -c board.c

clean:
	rm -rf *.o *~
