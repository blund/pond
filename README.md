# pond
<p align="center">
  <img src="https://image.shutterstock.com/image-vector/natural-pond-on-white-background-260nw-1166657947.jpg" alt="Placeholder logo"/>
</p>

Pond is a work in progress programming language based on the C language.

Its range is very limited at the moment, but if you want to try it out you can run the example file with
```
./run.sh
```
This only works on Linux, and you will need to have `GHC` (the Glasgow Haskell Compiler) and `GCC` (GNU Compiler Collection) installed, as well as the `run` utility (not the built in one in the shell!).

This will compile the file Examples/test.c, which looks something like this:
```
int main () {
    return -0b1001+0xff/(0b11+2);
}
```
We then use GCC to assemble this file, and produce and executable.
The backend currently compiles to x86_64. Other backends will be added when the language is more mature.

So far, the language assumes only the `main` function is defined, and that it `return`s a mathematical experssion. This expression supports decimal, binary and hexadecimal values :)

More will be added soon!

--

The project is currenctly following the blog series from Nora Sandler, [Writing a C Comiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html), which I heavily reccomend if you want to do a similar project.

The plan for the language is to explore haskell-like typeclasses in an imperative language, 'first class metaprogramming' and generally see what can be done at compile time. 
I also intend to build graphics/networking libraries in the future when when the language can do more than math.. But we'll get there!
