#!/bin/sh
# Baseline and prototype run concurrently, REPS times, so both see the same
# machine load. Compare the run= figures within each pair.
# Usage: profiling/abpair.sh NAME 'run_testsuite args' REPS PROTOTYPE...
here=$(cd "$(dirname "$0")" && pwd)
name=$1 args=$2 reps=$3
shift 3
r=1
while [ "$r" -le "$reps" ]; do
  "$here/abrun.sh" "$name-base-$r" "$args" &
  "$here/abrun.sh" "$name-new-$r" "$args" "$@" &
  wait
  r=$((r + 1))
done
