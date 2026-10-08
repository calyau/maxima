#!/bin/sh
# Time one run_testsuite with prototype redefinitions loaded.
# Usage: profiling/abrun.sh TAG 'run_testsuite args' [PROTOTYPE...]
# PROTOTYPE names a file in profiling/prototypes without .lisp. With PIN
# set, the run is pinned to that CPU. Prints run_testsuite's own CPU time
# (run), GC time (gc) and real time (real).
here=$(cd "$(dirname "$0")" && pwd)
out=${OUT:-$here/out}/ab
tag=$1 args=$2
shift 2
mkdir -p "$out"
loads=""
for p in "$@"; do
  loads="$loads:lisp (load \"$here/prototypes/$p.lisp\")
"
done
cd "$here/.." || exit 1
${PIN:+taskset -c "$PIN"} timeout 3600 ./maxima-local --no-init -q \
  --batch-string="$loads
run_testsuite($args);
" > "$out/$tag.log" 2>&1
rt=$(grep -oE '[0-9.]+ seconds of total run time' "$out/$tag.log" | cut -d' ' -f1)
gc=$(grep -oE 'Run times consist of [0-9.]+ seconds GC' "$out/$tag.log" | grep -oE '[0-9.]+')
re=$(grep -oE '[0-9.]+ seconds of real time' "$out/$tag.log" | cut -d' ' -f1)
ok=$(grep -cE 'No unexpected errors' "$out/$tag.log")
fails=$(grep -E 'problems? failed' "$out/$tag.log" | tr '\n' ' ')
echo "$tag run=$rt gc=$gc real=$re ok=$ok $fails"
