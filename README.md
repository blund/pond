# pond
<p align="center">
  <img src="https://image.shutterstock.com/image-vector/natural-pond-on-white-background-260nw-1166657947.jpg" alt="Placeholder logo"/>
</p>

Pond is a work in progress programming language based on the C language.

Its range is very limited at the moment, but if you want to try it out you can run the example file with 
```
run
```
This only works on Windows, and you will need to have GHC (the Glasgow Haskell Compiler) and GCC (GNU C Compiler) installed.

This will compile the file Examples/test.c, which looks something like this:
```
int main () {
    return -0b1001+0xff/(0b11+2);
}
```
You can view the result of this compilation in the file `out.s` after compilation.
We then use GCC to assemble this file, and produce and executable.
The backend currently compiles to x86. Other backends will be added when the language is more mature.

So far, the language assumes only the `main` function is defined, and that it `return`s a mathematical experssion. This expression supports decimal, binary and hexadecimal values :)

More will be added soon!
