#!/bin/sh
MAXIMA="@MAXIMA@"
LISP="@LISP@"
OUTPUT="@OUTPUT@"
PRELOAD=./mathmltest.mac

$MAXIMA --no-init --very-quiet --lisp=$LISP -p $PRELOAD --batch-string='mathml_batch("./example.mac");' \
	 | sed '/^;\+/{d}; /^read and interpret/{d}; /loadfile: /{d}; /^[[:space:]]\*$/{d};' > $OUTPUT

if test -r $OUTPUT; then
    grep -q '[Ee]rror' $OUTPUT
    if test $? -eq 0; then
	exit 1;
    fi
else
    exit 1;
fi
