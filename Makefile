all:
	ghc -dynamic src/*.hs src/**/*.hs 

run:
	src/main
