#!/usr/bin/env python3
"""Summarize abseq.sh output.

  abstat.py RESULTS [BASE]

RESULTS holds abrun.sh lines (TAG-NAME-ROUND run=.. gc=.. real=.. ok=..).
The warm-up line is ignored. For every config, prints the mean and range of
the CPU time over the rounds, and the change against the baseline config
BASE (default "base") computed per round, as mean and range over rounds.
"""
import re
import statistics
import sys
from collections import defaultdict

LINE = re.compile(r'(\S+)-(\d+) run=([\d.]+) gc=([\d.]+) real=([\d.]+) '
                  r'ok=(\d)\s*(.*)')


def main():
    path = sys.argv[1]
    base = sys.argv[2] if len(sys.argv) > 2 else 'base'
    runs = defaultdict(dict)
    notes = {}
    for line in open(path):
        m = LINE.match(line.strip())
        if not m:
            continue
        name, rnd, run, gc, real, ok, rest = m.groups()
        name = name.split('-', 1)[1] if '-' in name else name
        runs[name][int(rnd)] = (float(run), float(gc), float(real))
        if ok != '1':
            notes[name] = rest.replace('The following ', '') or 'no pass line'
    if base not in runs:
        sys.exit(f'no {base} runs in {path}')
    b = runs[base]
    bmean = statistics.mean(v[0] for v in b.values())
    print(f'| config | rounds | CPU s, mean [min, max] | GC s | '
          f'change vs {base}, per round | mean change | tests |')
    print('|---|---|---|---|---|---|---|')
    for name, rs in runs.items():
        cpu = [v[0] for v in rs.values()]
        gcs = [v[1] for v in rs.values()]
        deltas = [rs[r][0] - b[r][0] for r in sorted(rs) if r in b]
        if name == base:
            change = per = '–'
        else:
            md = statistics.mean(deltas)
            per = ', '.join(f'{d:+.1f}' for d in deltas)
            change = f'{md:+.1f} s ({100 * md / bmean:+.1f}%)'
        print(f'| {name} | {len(rs)} | {statistics.mean(cpu):.1f} '
              f'[{min(cpu):.1f}, {max(cpu):.1f}] | '
              f'{statistics.mean(gcs):.1f} | {per} | {change} | '
              f'{notes.get(name, "pass")} |')


if __name__ == '__main__':
    main()
