#!/usr/bin/env python3
"""Queries on the folded stacks written by prof.lisp (NAME.folded).

FILES is one folded file (.gz is fine), or several joined by commas
(e.g. two runs).

  stk.py FILES total
  stk.py FILES incl F...              samples with F anywhere on the stack
  stk.py FILES callers F [N]          frame above the outermost F
  stk.py FILES callees F [N]          frame below the outermost F
  stk.py FILES tree F [DEPTH] [MIN%]  call tree below the outermost F
  stk.py FILES files F [N]            test files contributing to F
  stk.py FILES leaves F [N]           innermost frames under F
  stk.py FILES attr F [N]             innermost Maxima frames under F
  stk.py FILES tables [N]             per test file, self, attributed

Frames are printed with *PACKAGE* MAXIMA, so Maxima symbols carry no
package prefix, and neither do CL symbols and SBCL assembler routines.
The files cl-names.txt and asm-names.txt next to this script list those.
SBCL keeps at most 62 frames per sample and marks the elided middle with
SB-SPROF::UNAVAILABLE-FRAMES, so inclusive counts are lower bounds.
"""
import gzip
import os
import sys
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
LIB_NAMES = set()
for fn in ('cl-names.txt', 'asm-names.txt'):
    try:
        with open(os.path.join(HERE, fn)) as f:
            LIB_NAMES.update(line.strip() for line in f)
    except OSError:
        pass
MAXIMA_PREFIXES = ('BIGFLOAT::', 'BIGFLOAT:', 'BIGFLOAT-IMPL:', 'CL-INFO::',
                   'INTL::', 'PREGEXP::', 'MT19937::', 'COMMAND-LINE::',
                   'SB-PCL::FAST-METHOD BIGFLOAT')


def load(paths):
    stacks = []
    for path in paths.split(','):
        opener = gzip.open if path.endswith('.gz') else open
        with opener(path, 'rt') as f:
            for line in f:
                line = line.rstrip('\n')
                i = line.rfind(' ')
                stacks.append((line[:i].split(';'), int(line[i + 1:])))
    return stacks


def is_maxima(name):
    if name.startswith('|TESTFILE') or name.startswith('<'):
        return False
    if name.startswith('foreign function'):
        return False
    core = name.lstrip('(')
    if core.startswith(MAXIMA_PREFIXES):
        return True
    head = core.split(' :IN ')[0].split(' ')[0]
    if ':' in head:
        return False
    return head not in LIB_NAMES


def is_gc(name):
    return 'maybe_gc' in name or 'collect_garbage' in name


def is_compiler(name):
    if 'WITH-COMPILATION-UNIT' in name:
        return False
    return (name.startswith('SB-C::') or name.startswith('(SB-C::')
            or ':IN SB-C::' in name)


def testfile(frames):
    for fr in frames:
        if fr.startswith('|TESTFILE '):
            return fr[10:-1]
    return '<none>'


def attribute(frames):
    if any(is_gc(f) for f in frames):
        return '<GC>'
    if any(is_compiler(f) for f in frames):
        return '<SBCL compiler>'
    for f in reversed(frames):
        if is_maxima(f):
            return f
    return '<no Maxima frame>'


def main():
    stacks = load(sys.argv[1])
    cmd = sys.argv[2]
    args = sys.argv[3:]
    total = sum(c for _, c in stacks)

    def pct(n):
        return 100.0 * n / total

    if cmd == 'total':
        print(total)
    elif cmd == 'incl':
        for f in args:
            n = sum(c for fr, c in stacks if f in fr)
            print(f'{n:8d} {pct(n):6.2f}%  {f}')
    elif cmd == 'tables':
        n = int(args[0]) if args else 40
        tables = {'test files': Counter(), 'self (innermost frame)': Counter(),
                  'attributed to innermost Maxima frame': Counter()}
        for fr, c in stacks:
            tables['test files'][testfile(fr)] += c
            tables['self (innermost frame)'][fr[-1]] += c
            tables['attributed to innermost Maxima frame'][attribute(fr)] += c
        print(f'{total} samples')
        for title, cnt in tables.items():
            print(f'\n{title}')
            for k, v in cnt.most_common(n):
                print(f'{pct(v):6.2f}%  {k}')
    elif cmd == 'tree':
        f = args[0]
        depth = int(args[1]) if len(args) > 1 else 4
        minpct = float(args[2]) if len(args) > 2 else 0.5
        counts = Counter()
        for fr, c in stacks:
            if f in fr:
                i = fr.index(f)
                path = tuple(fr[i:i + depth + 1])
                for d in range(1, len(path) + 1):
                    counts[path[:d]] += c
        kids = defaultdict(list)
        for k, v in counts.items():
            kids[k[:-1]].append((k, v))

        def rec(prefix, d):
            for k, v in sorted(kids[prefix], key=lambda kv: -kv[1]):
                if pct(v) >= minpct:
                    print(f'{"  " * d}{v:7d} {pct(v):6.2f}%  {k[-1]}')
                    if d < depth:
                        rec(k, d + 1)
        root = (f,)
        print(f'{counts[root]:7d} {pct(counts[root]):6.2f}%  {f}')
        rec(root, 1)
    elif cmd in ('callers', 'callees', 'files', 'leaves', 'attr'):
        f = args[0]
        n = int(args[1]) if len(args) > 1 else 30
        cnt = Counter()
        incl = 0
        for fr, c in stacks:
            if f not in fr:
                continue
            incl += c
            i = fr.index(f)
            if cmd == 'callers':
                key = fr[i - 1] if i > 0 else '<root>'
            elif cmd == 'callees':
                key = fr[i + 1] if i + 1 < len(fr) else '<self>'
            elif cmd == 'files':
                key = testfile(fr)
            elif cmd == 'leaves':
                key = fr[-1]
            else:
                key = attribute(fr[i:])
            cnt[key] += c
        print(f'{cmd} of {f}: {incl} samples ({pct(incl):.2f}%)')
        for k, v in cnt.most_common(n):
            print(f'{v:8d} {pct(v):6.2f}% {100.0 * v / max(incl, 1):6.1f}%rel  {k}')
    else:
        sys.exit(__doc__)


if __name__ == '__main__':
    main()
