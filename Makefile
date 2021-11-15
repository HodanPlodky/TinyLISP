all:
	ghc -dynamic src/*.hs src/**/*.hs -o main 

run:
	./main test.lisp
