
all:
	@ghc -e main Pond.hs
build:
	@ghc -o pond.exe Pond.hs
