#!/bin/sh
#
# Run Maxima with lisp-utils/check-expressions.lisp loaded and write a
# report of what the checks find. SBCL builds only.
#
# usage: lisp-utils/check-expressions.sh [options] [test ...]
#
#   -o BASE        write BASE.txt and BASE.sexp (default: ./exprcheck, or
#                  ./exprlint with --lint)
#   -j N           run N processes in parallel: one fresh process per
#                  test file, per manual file or per share of the fuzz
#                  cases. Some test files depend on state left by the
#                  ones before them, so their results can differ from a
#                  sequential run. Default: 1, and the test suite then
#                  runs in one process, in order, like run_testsuite().
#   --share        include the share test suite
#   --checks LIST  checks to turn on, as a Lisp list, for example
#                  "(:contract :result)"; see check-expressions.lisp
#   --examples     instead of the test suite, run the examples of the
#                  manual (the @c ===beg=== blocks in doc/info)
#   --fuzz N       instead of the test suite, run N random inputs
#   --lint         instead of running anything, lint the source with
#                  lint-expressions.lisp and write BASE.txt
#   --maxima PROG  the maxima-local to run (default: the one at the top
#                  of this tree)
#   test ...       run only these test files, named as in testsuite_files
#
# Output of the test suite goes to stdout. The checks never change a
# test result, so a test that fails here fails without them too.

set -e

ROOT=`cd "\`dirname "$0"\`/.." && pwd`
MAXIMA="$ROOT/maxima-local"
CHECKER="$ROOT/lisp-utils/check-expressions.lisp"
BASE=
JOBS=1
SHARE=false
CHECKS="maxima-exprcheck:*checks*"
MODE=tests
FUZZ=0
TESTS=""

while [ $# -gt 0 ]; do
    case "$1" in
        -o) BASE="$2"; shift 2 ;;
        -j) JOBS="$2"; shift 2 ;;
        --share) SHARE=true; shift ;;
        --checks) CHECKS="'$2"; shift 2 ;;
        --examples) MODE=examples; shift ;;
        --fuzz) MODE=fuzz; FUZZ="$2"; shift 2 ;;
        --lint) MODE=lint; shift ;;
        --maxima) MAXIMA="$2"; shift 2 ;;
        -h|--help) sed -n '3,30p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        -*) echo "$0: unknown option $1" >&2; exit 2 ;;
        *) TESTS="$TESTS $1"; shift ;;
    esac
done

if [ -z "$BASE" ]; then
    if [ $MODE = lint ]; then BASE=exprlint; else BASE=exprcheck; fi
fi
case "$BASE" in
    /*) ;;
    *) BASE=`pwd`/$BASE ;;
esac
mkdir -p "`dirname "$BASE"`"

# Each :lisp form sits on a line of its own, since :lisp takes the rest
# of its line.
LOAD=":lisp (progn (load \"$CHECKER\") (values))"
INSTALL=":lisp (maxima-exprcheck:install :checks $CHECKS)"

if [ $MODE = lint ]; then
    "$MAXIMA" --no-init -q --batch-string=":lisp (progn (load \"$ROOT/lisp-utils/lint-expressions.lisp\") (values))
:lisp (progn (format t \"~&exprlint: ~D hits, see ~A.txt~%\" (maxima-exprlint:lint \"$ROOT/\" \"$BASE.txt\") \"$BASE\") (values))
"
    exit 0
fi

# A Maxima list of strings from the words in $1.
maxima_list () {
    printf '['
    sep=''
    for t in $1; do printf '%s"%s"' "$sep" "$t"; sep=','; done
    printf ']'
}

if [ $MODE = tests ] && [ "$JOBS" -le 1 ]; then
    if [ -n "$TESTS" ]; then
        selection="tests=`maxima_list "$TESTS"`, "
    else
        selection=""
    fi
    "$MAXIMA" --no-init -q --batch-string="$LOAD
$INSTALL
run_testsuite(${selection}share_tests=$SHARE);
:lisp (progn (format t \"~&exprcheck: ~D groups, see ~A.txt~%\" (maxima-exprcheck:write-report \"$BASE\") \"$BASE\") (values))
"
    exit 0
fi

# Everything else runs as jobs: one process each, writing DIR/NAME.sexp,
# merged into BASE at the end. Some examples write files, so the jobs run
# in DIR/work.
DIR="$BASE.d"
rm -rf "$DIR"
mkdir -p "$DIR/work"
: > "$DIR/jobs"

# add_job NAME INPUT: INPUT is Maxima input run once the checks are on.
add_job () {
    printf '%s\n' "$1" >> "$DIR/jobs"
    printf '%s\n%s\n%s\n:lisp (maxima-exprcheck:write-report "%s/%s")\n' \
        "$LOAD" "$INSTALL" "$2" "$DIR" "$1" > "$DIR/$1.in"
}

case $MODE in
    tests)
        if [ -z "$TESTS" ]; then
            if [ "$SHARE" = true ]; then share=t; else share=nil; fi
            TESTS=`"$MAXIMA" --no-init -q --batch-string=":lisp (dolist (x (append (cdr maxima::\\$testsuite_files) (and $share (cdr maxima::\\$share_testsuite_files)))) (format t \\"~&exprcheck-test: ~A~%\\" (if (stringp x) x (second x))))
" | sed -n 's/^exprcheck-test: //p'`
        fi
        for t in $TESTS; do
            add_job "$t" "run_testsuite(tests=[\"$t\"], share_tests=$SHARE);"
        done ;;
    examples)
        for f in "$ROOT"/doc/info/*.texi "$ROOT"/doc/info/*.texi.m4; do
            case "$f" in
                *.m4) if [ -f "${f%.m4}" ]; then continue; fi ;;
            esac
            grep -q '^@c ===beg===' "$f" 2>/dev/null || continue
            add_job "`basename "$f"`" \
                ":lisp (maxima-exprcheck:run-texi-examples \"$f\")"
        done ;;
    fuzz)
        n=0
        size=$(( (FUZZ + JOBS - 1) / JOBS ))
        while [ $n -lt "$FUZZ" ]; do
            end=$(( n + size ))
            if [ $end -gt "$FUZZ" ]; then end=$FUZZ; fi
            add_job "fuzz-$n" ":lisp (maxima-exprcheck:run-fuzz $n $end)"
            n=$end
        done ;;
esac

export MAXIMA DIR
xargs -P "$JOBS" -I '{}' sh -c '
cd "$DIR/work" && "$MAXIMA" --no-init -q --batch-string="`cat "$DIR/{}.in"`" \
    > "$DIR/{}.log" 2>&1 || echo "exprcheck: {} exited with status $?"
' < "$DIR/jobs"

if [ $MODE = tests ]; then
    while read t; do
        r=`grep -E 'tests? passed|tests? failed|Caused an error break' "$DIR/$t.log" | tail -1`
        echo "$t: ${r:-no result, see $DIR/$t.log}"
    done < "$DIR/jobs"
fi

"$MAXIMA" --no-init -q --batch-string="$LOAD
:lisp (progn (format t \"~&exprcheck: ~D groups, see ~A.txt~%\" (maxima-exprcheck:merge-reports \"$BASE\" (directory \"$DIR/*.sexp\")) \"$BASE\") (values))
"
