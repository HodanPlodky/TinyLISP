all:
	ghc -dynamic src/*.hs src/**/*.hs -o main 

run:
	src/main
