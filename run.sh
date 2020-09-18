
case "$1" in
    tree)
        ghc -e getTree Main.hs
        ;;
    *)
        echo
        /usr/bin/time -f'ghc compile time: %es' ghc -O2 Main.hs -o ./Build/pond -odir Build -hidir Build &&
        /usr/bin/time -f'compilation time: %es' ./Build/pond Examples/test.pnd &&
        /usr/bin/time -f'assembly time: %es' gcc -g Build/out.s Foreign/print.c -o ./Build/a.out &&
        ./Build/a.out
        ;;
esac
