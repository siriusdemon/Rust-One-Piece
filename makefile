clean:
	rm -rf *.o
	rm -rf r2p gc

r2: runtime.o
	as -o r2.o r2.asm
	gcc r2.o runtime.o -o r2p

gc: runtime.o main.o
	gcc -o gc runtime.o main.o


runtime.o: runtime.c runtime.h
main.o: main.c runtime.c runtime.h
