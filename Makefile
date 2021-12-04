all:
	ghc -dynamic src/*.hs src/**/*.hs -o main 
	g++ -std=c++17 src/vm/*.cpp -o vm

run:
	./main test.lisp
