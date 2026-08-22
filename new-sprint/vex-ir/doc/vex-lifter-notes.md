ok, so this is the next-in-progress goal: "implement lifter from machine code (x86) to IR - use pyvex library"
and so implementing this requires two things, it requires using the pyvex lifter, which you have to install and setup and kind of understand what it is doing. And then it also requires understanding my Stroscot IR, and how the VEX IR is encoded in it

so notes:

- start by taking the example lifted prgoram from https://decompilation.wiki/fundamentals/cfg-recovery/lifting/#example-lifted-program 
```
int main(int argc) {
    int t;
    if (argc > 2) t = 0;
    else          t = 1;
    return t;
}
```
and figure out how to encode it in the Stroscot IR