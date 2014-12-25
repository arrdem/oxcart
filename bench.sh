#!/bin/bash

bench_one() {
       CP=`lein cp`
       OPTS="-cp $CP -server -XX:+TieredCompilation -XX:+AggressiveOpts -Xmx1024m"
       NS=$1

       echo "Running Clojure 1.6.0 compiled $NS...."
       time java $OPTS\
            -cp $CP\
            clojure.main\
            -m $NS

       echo "Oxcart compiling $NS...." &&
           lein run $NS &> /dev/null &&
           echo "Running Oxcart compiled $NS...." &&
           time java $OPTS\
                -cp $CP\
                $NS
}

bench_all() {
    for ns in `grep -h -r "ns" src/bench/ | awk '{print $2}' | sed 's/[()]//g'`;
    do
        bench_one "$ns"
    done
}

case $1 in
    "all")
        bench_all
        ;;
    *)
        bench_one $1
        ;;
esac
