
# Circular

Circular fixed-sized stacks.

Circular stacks with fxed maximum size are just normal vectors with a
pointer to the last element. They are useful because memory usage is constant,
and because they are fast, especially when summary statistics need to be
computed across the stack.

I use them, for example, as the data type for traces of Markov chains.

`Circular` is actively developed and functions may be removed, renamed, or
changed. New ideas are welcome!

