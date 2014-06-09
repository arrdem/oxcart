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

## Documentation & Caveats

Oxcart does not seek to compile all valid clojure programs. The
existing JVM Clojure reference implementation already does that
job. Oxcart is intended to compile a reasonable and valuable subset of
clojure programs which do not extensively leverage runtime dynamism
with an eye to whole program optimization and performance
improvements. Some unsupported dynamic behavior will be detected by
Oxcart and will result in compile time errors.

Unsupported forms include but are not limited to

 - `clojure.core/eval`
 - `clojure.core/load`
 - `clojure.core/read-string`
 - `clojure.core/resolve`
 - `clojure.core/alter-var-root`
 - `set!`

Oxcart seeks to perform a number of performance impacting
transformations, including the following

 1. Tree shaking for var elimination, including Clojure core and user specified libraries.
 2. Elimination of vars as a dynamic dispatch mechanism
 3. Elimination of function level implementation classes in favor of namespace or whole program level implementation classes.
 4. Inlining of functions

What do these program transformations mean? Quite simply that for some
configurations Oxcart bytecode is not compatible with reference JVM
Clojure implementation. An obvious example of this is function
inlining. On the reference JVM Clojure implementation, every function
has an implementing class and these are all loaded lazily at program
start. Instances of these classes are wrapped in `clojure.lang.Var`
instances and are interned in Clojure's global symbol table. This is
all well and good for a naive language implementation but it is
possible to do much better. By eliminating dynamic var access either
via `read-string`, `Var` or `eval`, Oxcart can statically prove that
your program will never take some function F as a value and thus that
it is wasteful to implement it as a fully fledged class when it could
be compacted. This means that attempts attempt to load an Oxcart
produced JAR or class from the REPL and interact with it as though it
were normal Clojure bytecode will almost certainly fail. Some Oxcart
configurations may provide full reference Clojure interop however this
behavior is low priority at present.

 - [Documentation](doc/index.org)
 - [Changelog](CHANGES.org)
 - [TODO list](TODO.org)

## License

Copyright © 2014 Reid McKenzie, Rich Hickey & contributors.

Distributed under the Eclipse Public License, the same as Clojure.
