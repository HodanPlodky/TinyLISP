all:
	ghc -v -dynamic src/**.hs

run:
	src/main
