#!/bin/sh
# Profile one run_testsuite with sb-sprof (see prof.lisp).
# Usage: profiling/runprof.sh NAME MODE INTERVAL 'run_testsuite args' [t|nil]
#   profiling/runprof.sh core-cpu :cpu 0.004 ''
#   profiling/runprof.sh full-cpu :cpu 0.004 'share_tests=true'
#   profiling/runprof.sh full-alloc :alloc 0.004 'share_tests=true' nil
# Output goes to $OUT (default profiling/out) as NAME.*.
here=$(cd "$(dirname "$0")" && pwd)
out=${OUT:-$here/out}
name=$1 mode=$2 iv=$3 args=$4 graph=${5:-t}
mkdir -p "$out"
cd "$here/.." || exit 1
timeout 3600 ./maxima-local --no-init -q --batch-string=":lisp (load \"$here/prof.lisp\")
:lisp (prof-start :mode $mode :interval $iv)
run_testsuite($args);
:lisp (prof-finish \"$out/$name\" :graph $graph)
" > "$out/$name.log" 2>&1
echo "$name exit=$?"
grep -E 'unexpected errors|tests failed|Lisp error' "$out/$name.log"
cat "$out/$name.summary"
