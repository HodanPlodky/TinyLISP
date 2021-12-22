all:
	ghc -dynamic src/*.hs src/**/*.hs -o main 
	g++ -std=c++20 -O0 -g src/vm/*.cpp -o vm

run:
	./main test.lisp
