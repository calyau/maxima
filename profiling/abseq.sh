#!/bin/sh
# Sequential, interleaved A/B timing. One discarded warm-up run of the
# first CONFIG, then ROUNDS rounds of all CONFIGs, the order rotated by one
# each round, so every config runs once per round and drift from other load
# spreads over all of them. Run nothing else meanwhile.
# Usage: profiling/abseq.sh TAG ROUNDS 'run_testsuite args' CONFIG...
# CONFIG is NAME=PROTO+PROTO..., or NAME= for the baseline, e.g.
#   PIN=3 profiling/abseq.sh full 3 'share_tests=true' base= sf=signfactor
# Result lines go to stdout, see abstat.py.
here=$(cd "$(dirname "$0")" && pwd)
tag=$1 rounds=$2 args=$3
shift 3
n=$#
first=$1
"$here/abrun.sh" "$tag-warmup" "$args" $(echo "${first#*=}" | tr '+' ' ')
r=1
while [ "$r" -le "$rounds" ]; do
  i=0
  while [ "$i" -lt "$n" ]; do
    k=$(( (i + r - 1) % n + 1 ))
    eval "cfg=\${$k}"
    "$here/abrun.sh" "$tag-${cfg%%=*}-$r" "$args" \
      $(echo "${cfg#*=}" | tr '+' ' ')
    i=$((i + 1))
  done
  r=$((r + 1))
done
