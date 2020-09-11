
echo
/usr/bin/time -f'ghc compile time: %es' ghc -O2 Main.hs -o pond -odir Build -hidir Build

/usr/bin/time -f'compilation time time: %es' ./pond Examples/test.c

/usr/bin/time -f'assembly time: %es' gcc out.s testing/print.c

./a.out

# rm a.out out.s pond
