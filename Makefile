
all:
	@ghc -e main Pond.hs
build:
	@ghc --make -outputdir build Pond.hs
compile:
	@ghc -e "import System.Environment" -e ":set args compile" -e main Pond.hs
