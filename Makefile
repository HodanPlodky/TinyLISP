all:
	ghc -dynamic src/*.hs src/**/*.hs -o main 
	g++ -std=c++20 -O2 -g src/vm/*.cpp -o vm

run:
	./main test.lisp
