# the Rio Lazy-ML compiler

Rio is, at its heart, a conservation effort. Before the lazy functional
programming community settled on Haskell (and nowadays GHC) as the
lingua franca for lazy evaluation, there were a several other lazy
languages: Miranda, SASL, KRC, and Lazy ML, to name a few.

The implmentation of Rio is based on Augustsson and Johnsson's work on
the Chalmers Lazy-ML compiler. We reuse many of the implementation
techniques described in that paper, and in Simon L. Peyton Jones' book
"Implementing Functional Languages: a tutorial".

Rio is to x86 assembly, using two intermediate languages: a "Core
language" that has had abstraction elimination performed (lambda
lifting), and an intermediate _G-machine_ for describing graph
reduction. Since compilation from the G-machine to x86 is almost
trivial, Rio produces very small executables.
