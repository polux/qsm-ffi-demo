# quickcheck-state-machine FFI demo

*Disclaimer: this is not an official Google product*

A Haskell implementation of the circular buffer example of the paper
[Experiences with QuickCheck: Testing the Hard Stuff and Staying
Sane](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quviq-testing.pdf)
using
[quickcheck-state-machine](http://hackage.haskell.org/package/quickcheck-state-machine).
Unlike the circular buffer example provided by quickcheck-state-machine, this
version uses the Haskell FFI to test a C implementation of the circular buffer,
like in the original paper.

Usage:

```
stack build
stack exec qsm-ffi-demo
```
