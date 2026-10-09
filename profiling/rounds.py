#!/usr/bin/env python3
"""Subsystem shares (frame anywhere on the stack), round 1 against round 2.
Run from the repository root."""
import os
import sys
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'stk.py')).read().split("def main")[0])
SP = os.environ.get('R2', 'profiling/data/round2')
R = {('r1', 'core'): 'profiling/data/core-cpu-1.folded.gz,profiling/data/core-cpu-2.folded.gz',
     ('r1', 'full'): 'profiling/data/full-cpu-1.folded.gz,profiling/data/full-cpu-2.folded.gz',
     ('r2', 'core'): SP + '/core-cpu-1.folded.gz,' + SP + '/core-cpu-2.folded.gz',
     ('r2', 'full'): SP + '/full-cpu-1.folded.gz,' + SP + '/full-cpu-2.folded.gz'}
def fp(fr): return any(f.startswith('FP') and f.isupper() and f.isidentifier() for f in fr)
CATS = [('`limit`', lambda fr: 'TOPLEVEL-$LIMIT' in fr or 'LIMIT' in fr),
        ('`sign` (`SIGN1`)', lambda fr: 'SIGN1' in fr),
        ('`MEQP`', lambda fr: 'MEQP' in fr),
        ('`ratsimp` (`FULLRATSIMP`/`SRATSIMP`)', lambda fr: 'FULLRATSIMP' in fr or 'SRATSIMP' in fr),
        ('`integrate`', lambda fr: 'INTEGRATE-IMPL' in fr),
        ('`factor`', lambda fr: 'FACTOR' in fr),
        ('bigfloat arithmetic (`FP*`)', fp),
        ('`taylor`', lambda fr: 'TAYLOR*' in fr),
        ('SBCL compiler at test time', lambda fr: any(is_compiler(f) for f in fr))]
res = {}
for k, p in R.items():
    st = load(p); tot = sum(c for _, c in st)
    res[k] = [100 * sum(c for fr, c in st if f(fr)) / tot for _, f in CATS]
print('| subsystem (frame on stack) | core r1 | core r2 | full r1 | full r2 |')
print('|---|---|---|---|---|')
for i, (name, _) in enumerate(CATS):
    print('| %s | %.1f%% | %.1f%% | %.1f%% | %.1f%% |' % (name, res[('r1','core')][i], res[('r2','core')][i], res[('r1','full')][i], res[('r2','full')][i]))
