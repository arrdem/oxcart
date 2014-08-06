#!/bin/bash
echo "Times in ms."
echo "Running Clojure 1.6.0 compiled test.vars...."
java -cp `lein cp`\
     -server -XX:+TieredCompilation\
     -XX:+AggressiveOpts\
     -jar ~/.m2/repository/org/clojure/clojure/1.6.0/clojure-1.6.0.jar\
     -i src/bench/clojure/test/vars.clj\
     -m test.vars | awk "{print \$3}"

echo "Oxcart compiling test.vars...." &&
lein run test.vars &> /dev/null &&
echo "Running Oxcart compiled test.vars...." &&
java -server\
     -XX:+TieredCompilation\
     -XX:+AggressiveOpts\
     -Xmx1024m\
     -cp `lein cp`\
     test.vars | awk "{print \$3}"

