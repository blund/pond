
case "$1" in
    tree)
        ghc -e getTree Main.hs
        ;;
    *)
        echo
        mkdir -p ./Build/CompilationFiles
        ghc Main.hs -o Build/pond -odir Build/CompilationFiles -hidir Build/CompilationFiles &&
        ./Build/pond Examples/test.pnd &&
        gcc -g Build/out.s Foreign/print.c -o ./Build/a.out &&
        ./Build/a.out
        echo $?
        ;;
esac
