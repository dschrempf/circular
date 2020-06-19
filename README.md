
# Circular

<p align="center"><img src="https://travis-ci.org/dschrempf/circular.svg?branch=master" /></p>

Circular fixed-sized stacks.

Circular stacks with fxed maximum size are just normal vectors with a
pointer to the last element. They are useful because

-   memory usage is constant
-   they are fast, especially when summary statistics need to be
    computed across the stack
-   they can be saved, and restored using JSON format

When the stack is full, new elements pushed on the stack replace the oldest
(deepest) elements on the stack. Complex circular behavior can arise when pushes
and pops are mixed. QuickCheck and unit tests with HSpec give promising results
&#x2014; have a look yourself.

I use them, for example, as the data type for traces of Markov chains.

`Circular` is actively developed and functions may be removed, renamed, or
changed. New ideas are welcome!

