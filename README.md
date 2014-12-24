# Project Oxcart

<center>
  <img src="./resources/oxcart.jpg">
  </img>
</center>

> Then, I heard it. The click of the mic button from the back
> seat. Very professionally, and with no emotion, Walter spoke: "Los
> Angeles Center, Aspen 20, can you give us a ground speed check?"
> There was no hesitation, and the replay came as if was an everyday
> request. "Aspen 20, I show you at one thousand eight hundred and
> forty-two knots, across the ground."
>
> –– Brian Schul, "Sled Driver: Flying the World's Fastest Jet"
>
> —– http://oppositelock.jalopnik.com/favorite-sr-71-story-1079127041

Oxcart is a prototype Clojure compiler which seeks to provide
aggressive AOT compilation for performance and memory usage. At present
Oxcart is a holding tank for changes which will likely be split up
between
[tools.emitter.jvm](https://github.com/clojure/tools.emitter.jvm),
[tools.analyzer.jvm](https://github.com/clojure/tools.analyzer.jvm)
and some future `tools.optimizer.jvm`.

Oxcart is currently on indefinite hiatus, pending changes to Clojure
addressing the current `clojure.lang.RT` `<cinit>` behavior of loading
clojure/core from source. See the blog posts for more information.

## Blog Posts

Stuff I read prior to starting Oxcart

 - [Clojure Compilation](http://nicholaskariniemi.github.io/2014/01/26/clojure-compilation.html)
 - [Clojure Compilation: Full Disclojure](http://nicholaskariniemi.github.io/2014/02/06/clojure-compilation2.html)
 - [Why is Clojure bootstrapping so slow?](http://nicholaskariniemi.github.io/2014/02/25/clojure-bootstrapping.html)

Oxcart posts

 - [Oxcart and Clojure](http://arrdem.com/2014/06/26/oxcart_and_clojure/)
 - [Of Oxen, Carts and Ordering](http://arrdem.com/2014/08/05/of_oxen,_carts_and_ordering/)
 - [lib-clojure](https://groups.google.com/forum/#!searchin/clojure-dev/lib-clojure/clojure-dev/dSPUNKSaV94/gTikbqYhJTYJ)
 - [Oxcart going forwards](http://arrdem.com/2014/12/11/oxcart_going_forwards/)

## Demos

The script "bench.sh" in the root of the Oxcart project is designed to
run the various Oxcart benchmarks with minimal effort. Usage is `bash
bench.sh NS` where `NS` is the namespace in the Oxcart source tree to
be run as a benchmark. That namespace will be loaded first with
Clojure 1.6, and then compiled & run via Oxcart. Runs are wraped in
`time` invocations, although several benchmarks do their own timing
internally.

### test.call

Usage: `bash bench.sh test.call`

This namespace is designed to assess the cost of making function calls
between Oxcart and core Clojure. It repeatedly invokes the
`test.call/vcall` function to perform primitive arithmetic and is
intended to measure the speedup from eliminating var indirection on
`test.call/vcall`.

### test.hello

Usage: `bash bench.sh test.hello`

This namespace just prints "Hello, World" as a test. Not actually a
benchmark, unless you want to measure how long it takes to boot a
Clojure instance, do minimal work and exit.

### test.load

Usage: `bash bench.sh test.load`

This namespace loads a slightly modified version of `test.vars`,
except that all the vars but `test.load/-main` are unreachable. This
is intended as a demonstration of how slow it is to compile programs
from source compared to loading them from compiled classes.

### test.vars

Usage: `bash bench.sh test.vars`

This namespace defines 502 functions, most of which are trivial, and
then calls a random set of them in a tight loop. This is also designed
to stress the costs of var indirection as the tight loop function is
much too large to reasonably inline across and the set of vars called
in each iteration is random on each pair of possible functions which
kills the branch predictor and increases the inlining cost.

## License

Copyright © 2014 Reid McKenzie.

Distributed under the Eclipse Public License, the same as Clojure.
