
all:
	@ghc -e main Main.hs
build: Main.hs
	@ghc --make -outputdir build -o Pond Main.hs
compile:
	@ghc -e "import System.Environment" -e ":set args compile" -e main Main.hs
