#!/bin/bash
TIMEOUT=1

echo "Benchmarking dedekind solver on all files in examples/. Timeout set to $TIMEOUT sec"

NRFILES=0
NRPROBS=0
NRSOLVED=0
NRTIMEOUT=0
RUNTIME=0

for f in examples/*.cube
do
    printf "Solving goals in $f ... "
    let "NRFILES+=1"
    cabal run dedekind -- -f "$f" -t $TIMEOUT >> output.txt
    let "NRPROBS+=$(grep '^SOLVING' output.txt | wc -l | grep -o "[0-9]\+")"
    let "NRSOLVED+=$(grep '^SOLVED' output.txt | wc -l | grep -o "[0-9]\+")"
    let "NRTIMEOUT+=$(grep '^TIMEOUT' output.txt | wc -l | grep -o "[0-9]\+")"
    if [ "$(grep '^SOLVED' output.txt | wc -l | grep -o "[0-9]\+")" -gt "0" ]; then
        let "RUNTIME+=$(grep '^SOLVED IN ' output.txt | grep -o "[0-9]\+ms" | grep -o "[0-9]\+" | awk '{ SUM += $1} END { print SUM }')"
    fi
    printf "$(grep '^SOLVED' output.txt | wc -l | grep -o "[0-9]\+")/$(grep '^SOLVING' output.txt | wc -l | grep -o "[0-9]\+") \\n"
    rm output.txt
done


echo "Found $NRPROBS problems in $NRFILES files"
echo "Solved $NRSOLVED problems, timeout of $TIMEOUT sec on $NRTIMEOUT problems"
echo "Spent on average $(bc <<<"scale=2; $RUNTIME / $NRSOLVED") ms on each problem, in total $RUNTIME ms"
