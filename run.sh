
echo
/usr/bin/time -f'ghc compile time: %es' ghc -O2 Main.hs -o ./Build/pond -odir Build -hidir Build

/usr/bin/time -f'compilation time time: %es' ./Build/pond Examples/test.c

/usr/bin/time -f'assembly time: %es' gcc out.s Foreign/print.c -o ./Build/a.out

./Build/a.out
