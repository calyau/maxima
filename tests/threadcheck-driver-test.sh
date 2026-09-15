#!/bin/sh
# Exercise the driver's pass/fail contract without depending on a Lisp's
# treatment of EOF, load errors or process exit codes.
set -eu

TEST_DIR=$(mktemp -d "${TMPDIR:-/tmp}/maxima-threadcheck-driver.XXXXXXXX")
trap 'rm -rf "$TEST_DIR"' 0
trap 'exit 2' 1 2 3 15
mkdir "$TEST_DIR/private logs"
TMPDIR="$TEST_DIR/private logs"
MAXIMA="$TEST_DIR/mock maxima"
export TMPDIR MAXIMA
cat > "$MAXIMA" <<'EOF'
#!/bin/sh
test "$#" -eq 3 || exit 99
test "$1" = --no-init || exit 99
test "$2" = --lisp=stub || exit 99
test "$3" = '--batch-string=:lisp (load (maxima-getenv "MAXIMA_THREADCHECK_FILE"))' || exit 99
test -f "$MAXIMA_THREADCHECK_FILE" || exit 99
printf '%s\n' "$MOCK_OUTPUT"
exit "$MOCK_STATUS"
EOF
chmod +x "$MAXIMA"

checks=0
for mode in bindings race; do
    for MOCK_STATUS in 0 1 2 77 93 127 255; do
        for MOCK_OUTPUT in '' 'threadcheck: PASS' 'threadcheck: SKIP' \
            'threadcheck: FAIL' 'threadcheck: PASS trailing text' \
            'Maxima encountered a Lisp error'; do
            export MOCK_STATUS MOCK_OUTPUT
            expected=$MOCK_STATUS
            if test "$MOCK_STATUS" -eq 0; then
                expected=1
                if test "$MOCK_OUTPUT" = 'threadcheck: PASS'; then
                    expected=0
                fi
            elif test "$MOCK_STATUS" -eq 77; then
                expected=1
                if test "$mode" = race &&
                    test "$MOCK_OUTPUT" = 'threadcheck: SKIP'; then
                    expected=77
                fi
            fi
            actual=0
            ./threadcheck.sh stub "$mode" >"$TEST_DIR/output" 2>&1 || actual=$?
            if test "$actual" -ne "$expected"; then
                cat "$TEST_DIR/output"
                echo "mode=$mode status=$MOCK_STATUS output=$MOCK_OUTPUT" >&2
                echo "Expected status $expected; got $actual" >&2
                exit 1
            fi
            checks=$((checks + 1))
        done
    done
done

for mode in '' unknown; do
    actual=0
    # With no mode, deliberately test the missing-argument path.
    if test -z "$mode"; then
        ./threadcheck.sh stub >"$TEST_DIR/output" 2>&1 || actual=$?
    else
        ./threadcheck.sh stub "$mode" >"$TEST_DIR/output" 2>&1 || actual=$?
    fi
    test "$actual" -eq 2
    checks=$((checks + 1))
done

# Every success, skip and failure path must remove its temporary log.
test -z "$(ls -A "$TMPDIR")"
echo "threadcheck driver: $checks status/marker cases passed"
