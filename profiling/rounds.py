#!/usr/bin/env python3
"""Subsystem shares (frame anywhere on the stack), rounds 1 to 3.
Run from the repository root."""
import os
import sys
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'stk.py')).read().split("def main")[0])
SP = os.environ.get('R2', 'profiling/data/round2')
S3 = os.environ.get('R3', 'profiling/data/round3')
R = {('r1', 'core'): 'profiling/data/core-cpu-1.folded.gz,profiling/data/core-cpu-2.folded.gz',
     ('r1', 'full'): 'profiling/data/full-cpu-1.folded.gz,profiling/data/full-cpu-2.folded.gz',
     ('r2', 'core'): SP + '/core-cpu-1.folded.gz,' + SP + '/core-cpu-2.folded.gz',
     ('r2', 'full'): SP + '/full-cpu-1.folded.gz,' + SP + '/full-cpu-2.folded.gz',
     ('r3', 'core'): S3 + '/core-cpu-1.folded.gz,' + S3 + '/core-cpu-2.folded.gz',
     ('r3', 'full'): S3 + '/full-cpu-1.folded.gz,' + S3 + '/full-cpu-2.folded.gz'}
def fp(fr): return any(f.startswith('FP') and f.isupper() and f.isidentifier() for f in fr)
CATS = [('`limit`', lambda fr: 'TOPLEVEL-$LIMIT' in fr or 'LIMIT' in fr),
        ('`sign` (`SIGN1`)', lambda fr: 'SIGN1' in fr),
        ('`MEQP`', lambda fr: 'MEQP' in fr),
        ('`ratsimp` (`FULLRATSIMP`/`SRATSIMP`)', lambda fr: 'FULLRATSIMP' in fr or 'SRATSIMP' in fr),
        ('`integrate`', lambda fr: 'INTEGRATE-IMPL' in fr),
        ('`factor`', lambda fr: 'FACTOR' in fr),
        ('bigfloat arithmetic (`FP*`)', fp),
        ('`taylor`', lambda fr: 'TAYLOR*' in fr),
        ('SBCL compiler at test time', lambda fr: any(is_compiler(f) for f in fr)),
        ('`SIGNDIFF-SPECIAL`', lambda fr: 'SIGNDIFF-SPECIAL' in fr),
        ('`SIGN-MABS`', lambda fr: 'SIGN-MABS' in fr),
        ('`SIGN-LOG`', lambda fr: 'SIGN-LOG' in fr),
        ('binding (`MBIND-DOIT`, `MUNBIND`)', lambda fr: 'MBIND-DOIT' in fr or 'MUNBIND' in fr),
        ('`SCALARCLASS`', lambda fr: 'SCALARCLASS' in fr),
        ('limit cache (`GETLIMVAL`, `PUTLIMVAL`)', lambda fr: 'GETLIMVAL' in fr or 'PUTLIMVAL' in fr)]
res = {}
for k, p in R.items():
    st = load(p); tot = sum(c for _, c in st)
    res[k] = [100 * sum(c for fr, c in st if f(fr)) / tot for _, f in CATS]
RS = ('r1', 'r2', 'r3')
print('| subsystem (frame on stack) | ' + ' | '.join('%s %s' % (s, r) for s in ('core', 'full') for r in RS) + ' |')
print('|---|' + '---|' * (2 * len(RS)))
for i, (name, _) in enumerate(CATS):
    print('| %s | ' % name + ' | '.join('%.1f%%' % res[(r, s)][i] for s in ('core', 'full') for r in RS) + ' |')
