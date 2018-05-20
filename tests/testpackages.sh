#!/bin/bash
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.


# That script tries to find all Maxima 'packages' and load them
# - once
# - twice
# - any combination of two packages
# to find incompatibilities, not working packages, etc.

# Usage: Call
# tests/testpackages.sh
# from the Maxima source directory (the script expects a compiled 
# Maxima there (./maxima-local is called...)

# The question is - what is really a "package" and what is just a
# helper file for other packages?
# currently all Maxima ".mac" files are loaded
# (except for some testing and demo files)
#
# Yes, I know, Lisp files should also be considered.

# Some packages create a gnuplot window, so the script requires a working X Display
# (and can't (currently) be run from a cron-job).

# Do everything in English
export LANG=C

packages=""

maxima="./maxima-local"

for i in share/**/*.mac ; do
    package1=$(echo "$i" | sed s+.*/++ | sed s+.mac$++)
    if [[ ${package1:0:5} != "rtest" && ${package1:0:5} != "test-" && ${package1:0:4} != "prob" && $package1 != "tendemo" && $package1 != "simplify_sum_test" && $package1 != "test_orthopoly" ]] ; then
        packages="$packages $package1"
    fi
done

true >report-single-packages.txt
# test single packages
for i in $packages ; do
    echo "---------------- Testing: $i ----------------"
    $maxima --quiet --run-string="load(\"$i\");quit();"
done </dev/null >>report-single-packages.txt 2>&1
echo "Loading single package test finished"

true >report-packages-twice.txt
# test loading packages twice
for i in $packages ; do
    echo "---------------- Testing: $i twice ----------------"
    $maxima --quiet --run-string="load(\"$i\");load(\"$i\");quit();"
done </dev/null >>report-packages-twice.txt 2>&1
echo "Loading packages twice test finished"

# The combination test runs very long.
# If you do not need it, you may want to insert an "exit" commańd here..

true >report-packages-combination.txt
# test loading a combination of packages
for i in $packages ; do
    for j in $packages ; do
        echo "---------------- Testing: $i and $j ----------------"
        $maxima --quiet --run-string="load(\"$i\");load(\"$j\");quit();"
    done
done </dev/null >>report-packages-combination.txt 2>&1
echo "Loading package combination test finished"
