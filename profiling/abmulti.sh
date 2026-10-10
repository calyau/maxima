#!/bin/sh
# Interleaved A/B timing of separate builds. One discarded warm-up run per
# build (fills its objdir), then ROUNDS rounds of all builds, the order
# rotated by one each round. Pinned to CPU $PIN (default 3).
# Usage: abmulti.sh TAG ROUNDS 'run_testsuite args' NAME=DIR...
# Result lines (TAG-NAME-ROUND run=.. gc=.. real=.. ok=..) go to stdout,
# for profiling/abstat.py.
tag=$1 rounds=$2 args=$3
shift 3
out=${OUT:?set OUT}/ab
mkdir -p "$out"
run1() { # LABEL DIR
  ( cd "$2" && taskset -c "${PIN:-3}" timeout 3600 ./maxima-local --no-init -q \
      --batch-string="run_testsuite($args);" ) > "$out/$1.log" 2>&1
  rt=$(grep -oE '[0-9.]+ seconds of total run time' "$out/$1.log" | cut -d' ' -f1)
  gc=$(grep -oE 'Run times consist of [0-9.]+ seconds GC' "$out/$1.log" | grep -oE '[0-9.]+')
  re=$(grep -oE '[0-9.]+ seconds of real time' "$out/$1.log" | cut -d' ' -f1)
  ok=$(grep -cE 'No unexpected errors' "$out/$1.log")
  echo "$1 run=$rt gc=$gc real=$re ok=$ok"
}
for cfg in "$@"; do run1 "$tag-warmup_${cfg%%=*}" "${cfg#*=}"; done
n=$#
r=1
while [ "$r" -le "$rounds" ]; do
  i=0
  while [ "$i" -lt "$n" ]; do
    k=$(( (i + r - 1) % n + 1 ))
    eval "cfg=\${$k}"
    run1 "$tag-${cfg%%=*}-$r" "${cfg#*=}"
    i=$((i + 1))
  done
  r=$((r + 1))
done
