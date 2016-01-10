#!/bin/bash
CP=`lein cp`
OPTS="-cp $CP -server -XX:+TieredCompilation -XX:+AggressiveOpts -Xmx1024m"

for NS in `grep -r "ns test." src/ | awk '{print $2}' | sed -e 's/[()]//g'`
do
    echo "Running Clojure 1.6.0 compiled $NS...." &&
    time java $OPTS\
         -cp $CP\
         clojure.main\
         -m $NS

    echo "Oxcart compiling $NS...." &&
    lein run $NS &> /dev/null &&
    echo "Running Oxcart compiled $NS...." &&
    time java $OPTS\
         -cp $CP\
         $NS || exit
done
