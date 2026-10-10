# Maxima hot spots: sb-sprof profile of the test suites (SBCL 2.6.9)

Maxima master at `4e18758`, built with SBCL 2.6.9 (official x86-64 Linux
binary, released 2026-09-26, checksum verified against the signed release
file): `sh bootstrap && ./configure --enable-sbcl && make`.

## Method

- Suites: core `run_testsuite()` (16,581 tests) and full
  `run_testsuite(share_tests=true)` (21,670 tests). All runs reported
  `No unexpected errors`.
- One unprofiled warm-up run first, so share packages built through
  `.system` files are already compiled into the objdir.
- `sb-sprof` in `:cpu` mode, 4 ms interval, two runs per suite (results agree
  within ~0.2 points), plus one `:alloc` run of the full suite. CPU-mode
  sampling cannot go below the kernel tick here (HZ=250, so 1 and 2 ms
  intervals still sample every 4 ms).
- `prof.lisp` replaces `TEST-BATCH` by a wrapper that runs each test file
  inside a compiled function named `|TESTFILE <name>|`, so every sample's
  stack names its test file.
- "Attributed" time charges each sample to the innermost Maxima function on
  the stack, so time spent in `GET`, bignum or list primitives lands on the
  Maxima function that called them.
- Candidate fixes were prototyped as runtime redefinitions (`prototypes/`)
  and timed with `abseq.sh`: one discarded warm-up run, then rounds in which
  the baseline and every prototype run once each, sequentially, in an order
  rotated per round, pinned to one CPU with nothing else running. Times are
  `run_testsuite`'s own CPU time. Each change is taken against the baseline
  of the same round and given as mean ± standard error over the rounds.

Caveats:

- SBCL keeps at most 62 frames per sample (`MAX_RECORDED_TRACE_LEN` in
  `src/runtime/sprof.c`) and elides the middle of deeper stacks. 56% of the
  samples are truncated. Self and attributed numbers are exact, inclusive
  numbers are lower bounds.
- Samples falling inside a GC are deferred and collapse into one, so GC is
  under-sampled (3.5% of samples). The GC share below comes from
  `sb-ext:*gc-run-time*`.

## Totals

|                                     | core      | full      |
|-------------------------------------|-----------|-----------|
| CPU time (profiled, 2 runs)         | 85–87 s   | 150 s     |
| CPU time (unprofiled, A/B host)     | 72.2 s    | 130.6 s   |
| GC time                             | 5.1%      | 6.1%      |
| bytes consed                        | 36.4 GB   | 67.6 GB   |
| SBCL compiler running at test time  | 2.5%      | 5.2%      |

The container was restarted between profiling and the A/B runs, and the
new host is about 13% faster, so compare times only within one table.

Test files with the largest share of CPU samples:

| core                  |       | full                   |       |
|-----------------------|-------|------------------------|-------|
| rtest_integrate       | 19.9% | rtest_integrate        | 11.5% |
| rtest_limit_extra     | 15.4% | rtest_abs_integrate    |  9.9% |
| rtest_limit_gruntz    |  7.9% | rtest_limit_extra      |  9.0% |
| rtest_limit           |  7.4% | rtest_matrixexp        |  6.8% |
| rtest_trig            |  7.2% | rtest_limit_gruntz     |  4.5% |
| rtest16               |  4.7% | rtest_limit            |  4.2% |
| rtest_gamma           |  4.3% | rtest_trig             |  4.1% |
| rtest15               |  4.0% | rtest_cholesky         |  3.4% |
| rtest14               |  3.7% | rtest16, rtest_orthopoly | 2.7% each |
| rtestint              |  3.5% | rtest_odelin           |  2.6% |

Subsystems, inclusive (lower bounds, overlapping):

| subsystem (frame on stack)              | core  | full  |
|-----------------------------------------|-------|-------|
| `limit`                                 | 33.0% | 19.4% |
| `sign` (`SIGN1`)                        | 24.9% | 19.9% |
| `ratsimp` (`FULLRATSIMP`/`SRATSIMP`)    | 21.0% | 20.2% |
| CRE polynomial arithmetic (`rat3*`)     | 20.0% | 22.9% |
| `integrate`                             | 17.5% | 14.2% |
| `factor`                                | 15.7% | 11.3% |
| bigfloat arithmetic (`FP*`)             |  9.4% |  6.6% |
| `taylor`                                |  6.8% |  4.2% |

Leaf cost by kind (share of all samples whose innermost frame is of that kind):

| kind                                                     | core  | full  |
|----------------------------------------------------------|-------|-------|
| property-list access (`GET3`, `SYMBOL-PLIST`, `GETL`, `MGET`, `%PUT`, ...) | 11.2% | 13.8% |
| bignum primitives (`SB-BIGNUM`)                          |  9.6% |  6.2% |
| generic arithmetic (`TWO-ARG-*`, `FLOOR`, `TRUNCATE`, ...) |  5.2% |  5.2% |
| list primitives (`MEMBER`, `DELETE`, `NCONC`, `LENGTH`, ...) |  5.1% |  6.5% |
| `SET`/`MAKUNBOUND` checks and globaldb lookups            |  1.7% |  1.7% |
| `EQUAL`                                                  |  1.5% |  1.3% |

Top attributed Maxima functions (full suite): SBCL compiler 5.2%, GC 3.5%,
`SIMPLIFYA` 3.4%, `ALIKE1` 3.3%, `MEVAL1` 2.5%, `PCTIMES` 2.2%, `GETL` 2.1%,
`MOPP1` 1.7%, `FPQUOTIENT` 1.6%, `TIMESIN` 1.5%, `EQTEST` 1.5%, `ALIKE`
1.5%, `PSIMP` 1.5%, `NORMALIZED-MODULUS` 1.4%, `ORDERPOINTER` 1.4%. Core:
`ALIKE1` 4.8%, `SIMPLIFYA` 3.5%, `PCTIMES` 3.2%, `FPQUOTIENT` 2.5%, `ALIKE`
2.4%, `FPROUND` 1.9%, `EQTEST` 1.8%, `MEVAL1` 1.7%. Full tables are in
`data/tables-*.txt`.

## Hot spots, ranked

Run to run, the unprofiled baseline varies by about ±2%, so three rounds of
a whole suite resolve changes of a few seconds only. Prototypes with smaller
effects were measured again on the test files that carry their hot spot,
with ten rounds each.

Whole suites, three rounds:

| prototype (`prototypes/`)                  | item | full, base 130.6 s     | core, base 72.2 s      | tests     |
|--------------------------------------------|------|------------------------|------------------------|-----------|
| `signfactor`: no `factor` for linear sums  | 1    | −6.5 ± 0.4 s (−5.0%)   | −5.1 ± 0.8 s (−7.1%)   | pass      |
| `fpsqrt`: `isqrt` bigfloat square root     | 3    | −0.3 ± 2.3 s           | −2.7 ± 0.8 s (−3.8%)   | 8 change  |
| `dollarify`: memoized `DOLLARIFY`          | 4    | +1.4 ± 2.7 s           | −1.9 ± 1.6 s           | pass      |
| `gc`: 4× `bytes-consed-between-gcs`        | 10   | −1.9 ± 2.4 s           | −1.4 ± 0.5 s (−1.9%)   | pass      |
| `mbind`: cheaper binding/unbinding         | 6    | −0.3 ± 1.8 s           | +1.5 ± 1.3 s           | pass      |
| `simplifya`: one plist pass per dispatch   | 2    | −2.6 ± 2.2 s           | −1.7 ± 1.7 s           | pass      |
| all six together                           |      | −17.0 ± 2.2 s (−13.0%) | −11.0 ± 1.8 s (−15.2%) | 8 change  |

Targeted, ten rounds:

| prototype   | test files                                    | base   | change                  |
|-------------|-----------------------------------------------|--------|-------------------------|
| `fpsqrt`    | `rtest14`, `rtest_hg`, `rtest_gamma`          | 7.9 s  | −2.6 ± 0.1 s (−34%)     |
| `mbind`     | `rtest_integrate`, `rtest_abs_integrate`      | 26.4 s | −0.9 ± 0.3 s (−3.3%)    |
| `dollarify` | `rtest_trig`, `rtest_limit`, `rtest_limit_gruntz` | 15.4 s | −0.3 ± 0.4 s         |
| `simplifya` | `rtest_integrate`, `rtest_abs_integrate`      | 26.4 s | −0.3 ± 0.3 s            |
| `gc`        | `rtest_integrate`, `rtest_abs_integrate`      | 26.4 s | −0.2 ± 0.3 s            |

So `signfactor`, `fpsqrt` and `mbind` are measurably faster, and the six
together save 13% of the full and 15% of the core suite. `dollarify` is too
small for these runs but can be costed directly (item 4). `simplifya` and
`gc` show no effect beyond the noise. All runs are in `data/ab-results.txt`.

### 1. `sign` factors every undecided sum (`SIGNFACTOR` → `FACTOR-IF-SMALL`)

Core ≥12.9% inclusive, full ≥9.1%. `SIGN-MPLUS` (`src/compar.lisp`) calls
`SIGNFACTOR` whenever `SIGNDIFF` and `SIGNSUM` cannot decide, and
`FACTOR-IF-SMALL` runs the full `factor` on any sum with `conssize < 51`.
Instrumented over the full suite:

| `SIGNFACTOR` calls                         | count   | time   |
|--------------------------------------------|---------|--------|
| linear sum, no sign gained                 | 779,545 | 12.3 s |
| non-linear sum, no sign gained             | 139,434 |  5.0 s |
| linear sum, sign gained                    |   2,904 |  0.1 s |
| non-linear sum, sign gained                |   2,246 |  0.1 s |

("Linear" here: every term is a number, a kernel, or a number times a kernel.
Nested calls make the time column overlap a little.) For a linear sum
`factor` can only pull out a numeric content (`2*a-2*b` → `2*(a-b)`, which
`SIGNDIFF` then decides), and the comparison below shows that this is all
the 2,904 linear gains needed.

Prototype `prototypes/signfactor.lisp`: for linear sums with rational
coefficients, skip `factor` and build `c*(x/c)` directly, with the same sign
normalization `factor` uses (coefficient of the greatest term positive, and a
`((mtimes simp) -1 ...)` product that `MUL` would otherwise distribute away).
Running old and new side by side on every call of the full suite, the
resulting sign differed in 7 of ~924k calls, all sums containing a `log`
whose argument `factor` rewrites (old `$complex`, new `$pnz`). No test
changed. A/B: −6.5 ± 0.4 s (−5.0%) on the full suite and −5.1 ± 0.8 s
(−7.1%) on the core suite, all tests pass.

The 139k non-linear calls that gained nothing cost another 5 s, so a cheap
test for when factoring can pay off would be the next step.

### 2. Property-list lookups

11–14% of all samples end in `GET3`/`SYMBOL-PLIST`/`GETL` and friends.
`GET` is a linear walk, and Maxima's hot symbols carry long plists (`MTIMES`
22 properties, `%SIN` 24, `MEQUAL` 14, `MLIST` 13), so lookups of absent
properties walk the whole list. Main callers (share of full-suite `GET3`):
`SIMPLIFYA` 22%, `MOPP1` 10%, `EQTEST` 7%, `PSIMP` 6%, `GETL-LM-FCN-PROP`,
`MSET`, `MEVAL1`, `MGET`, `SAFE-MGET`, `KINDP` 3–4% each.

- `SIMPLIFYA` does three separate `GET`s (`distribute_over`, `opers`,
  `operators`) per dispatch. Over the core suite that is 51M dispatching
  calls walking 542M plist cells: 8.6–8.7 per call for `MPLUS`/`MTIMES`,
  42–47 for `MLIST`, `MEQUAL`, `MABS` and the relational operators (three
  full misses). Prototype `prototypes/simplifya.lisp` collects all three in
  one pass. A/B: no effect beyond the noise (−0.3 ± 0.3 s on
  `rtest_integrate` and `rtest_abs_integrate`), all tests pass, so the walks
  themselves are a small part of what `GET3` costs here.
- `MEVAL1` function dispatch, see item 6.
- `EQTEST` re-reads `msimpind` for every simplified sum and product.

### 3. Bigfloat square root (`FPROOT`)

Core 4.7%, full 2.7% inclusive, nearly all from `bigfloat:sqrt` (inside
`HYPERGEOMETRIC-BY-SERIES`, which takes up to three complex `abs` per term, and
`EXPTBIGFLOAT`). `FPROOT` (`src/float.lisp`) runs Newton iterations from a
power-of-two start with a full-precision `FPQUOTIENT` per step. For n = 2,
`isqrt` of the shifted mantissa plus a sticky bit and one `FPROUND`
(`prototypes/fpsqrt.lisp`) is 8–19× faster per call (56 to 3,333 bits) and
correctly rounded, while the current `FPROOT` result is 1 ulp off in about 19%
of random arguments at every precision tested. A/B: 7.9 → 5.2 s on `rtest14`,
`rtest_hg` and `rtest_gamma` (−2.6 ± 0.1 s), −2.7 ± 0.8 s on the core suite.
Eight tests
change: `rtest_gamma` 742–743, `rtest_elliptic` 75, 235, 245, 250, 254 and
`rtest_limit_extra` 187. All compare at or near the last bit of the old results,
so they would need re-baselining.

Separately, `HYPERGEOMETRIC-BY-SERIES` only needs `abs` for a running error
bound, where `|re|+|im|` (or a double-float estimate) would avoid the square
roots altogether.

### 4. `def-simplifier` builds an error-message name on every call

1.8% full, 2.4% core inclusive. The simplifier body generated by
`def-simplifier` (`src/defmfun-check.lisp`) evaluates
`` `((,noun-name) ,@(rest (dollarify ',lambda-list))) `` before
`ARG-COUNT-CHECK`, on every call of `simp-%log`, `simp-%sin`, `simp-%cos`,
`simp-%atan2`, `cabs` and the rest. `DOLLARIFY` prints and re-reads each symbol
(`MEXPLODEN`, `READLIST`, `MAKEALIAS`). The name is only needed when the
arity is wrong. Fix: compute it inside the error branch or at macroexpansion
time. The full suite makes 2.0M `DOLLARIFY` calls at about 1.0 µs each, so
the fix is worth about 2 s per full run (1.5%), all tests pass with the
memoized prototype. That is below what three rounds can resolve, and ten
rounds on `rtest_trig`, `rtest_limit` and `rtest_limit_gruntz` (623k calls,
about 0.6 s) measured −0.3 ± 0.4 s.

### 5. Share packages loaded as Lisp source

`load()` of a `.lisp` file goes through `LOADFILE` → `cl:load` of the source,
so SBCL's native compiler runs on every form at every load. Runtime
compilation by cause:

| cause                                              | core  | full  |
|----------------------------------------------------|-------|-------|
| `load()` of `.lisp` source (`LOADFILE` on stack)   | 0.97% | 2.71% |
| other source loads (`LOADFILE` lost to truncation) | 0.49% | 1.04% |
| rule definitions (`META-FSET`)                     | 0.57% | 0.62% |
| `translate`                                        | 0.09% | 0.09% |
| rest (truncated stacks below `MEVAL1`, `compile`)  | 0.40% | 0.76% |

Only packages built through `.system` files get objdir fasls. Heaviest during
the full suite: `fourier_elim.lisp`, `stringproc.lisp`,
`to_poly_solve_extra.lisp`, `grobner.lisp`, `numdistrib.lisp`, `bitwise.lisp`,
`itensor.lisp`, `pslq_integer_relation.lisp` (0.3–0.5 s each).
`load("hypergeometric")` (`rtest_hg`, `rtest_nfloat`, `abs_integrate.mac`)
recompiles `src/hypergeometric.lisp`, which is already in the image. Compiling
share `.lisp` files into the objdir on first load, as `.system` packages are,
would remove almost all of this, also for users.

### 6. Maxima interpreter: call dispatch and variable binding

Samples attributed to the evaluator and its helpers (`MEVAL1`, `GETL`,
`MGET`, `GETL-LM-FCN-PROP`, `MLAMBDA`, `MSET`, `MBIND-DOIT`, `MUNBIND`, ...)
add up to 8.6% of the core and 11.8% of the full suite, where share packages
written in Maxima (`abs_integrate`, `to_poly_solve`, ...) run interpreted.

Dispatch: `MEVAL1` probes `noun`, `translated-mmacro`, `trace` (`MGET`),
`translated`, `mexpr`/`mmacro` (`MGETL`), `mfexpr*`, then `subr`/`macro` for
every Maxima-level call, mostly missing. `GETL` called from `MEVAL1` alone is
1.9% of the full suite. Ordering the probes by frequency, or caching the
dispatch per symbol with invalidation on `putprop`/`remprop`, would cut most
of it.

Binding: `MBIND-DOIT` + `MUNBIND` ≈ 3.8% of the full suite. Per bound
variable:

- `MBIND-DOIT` calls `$listofvars` on the variable to check that it is a
  symbol (0.5% full),
- `MSET` → `ADD2LNC` (`memalike` over `$values`, then `nconc` to its end) and
  `OPTIONP` (`member` over `$values` and `$labels`), 1.3% full,
- `MUNBIND-MAKUNBOUND` deletes the variable from `$values` with generic
  `DELETE` (0.9% full),
- `SET`/`MAKUNBOUND` pay SBCL's `ABOUT-TO-MODIFY-SYMBOL-VALUE` globaldb
  checks.

Prototype `prototypes/mbind.lisp` (symbol fast path, hand-written delete):
−0.9 ± 0.3 s (−3.3%) on `rtest_integrate` and `rtest_abs_integrate`, which
carry 60% of the binding cost, all tests pass. Going further needs a
different structure for `$values` membership, such as a hash set kept beside
the list.

### 7. Limit cache as an alist (`GETLIMVAL`/`PUTLIMVAL`)

1.6% full, 2.8% core, two thirds of it in `rtest_limit_gruntz`.
`limit-answers` is an alist searched by `ASSOLIKE` (`ALIKE1` on whole
expressions). A hash table keyed on a structural hash that ignores header
flags would make lookups O(1).

### 8. `SCALARCLASS` re-walks subexpressions

1.7% full, almost all in `rtest_cholesky`, ending in `MOPP1` (`(get fun
'op)` plus a `member` over `$props` per operator). `$props` was only 43 long
there, so this is call volume. For a sum or product `SCALARCLASS`
(`src/simp.lisp`) calls `CONSTTERMP` on each term, which runs `$constantp`
over the whole term and then `$nonscalarp`, that is `SCALARCLASS` again, and
falls back to `SCALARCLASS-LIST`, which classifies the terms once more. Each
level re-traverses the subtrees below it, so the cost grows faster than the
expression. One recursive pass returning both constness and scalar class
would make it linear.

### 9. Rule definitions compile natively

`defrule`, `tellsimp`, `tellsimpafter`, `defmatch` pass their generated
matcher through `META-FSET` → `EVAL` → SBCL's compiler: 0.6% of both suites.
Compiling lazily on first use, or evaluating with `sb-eval`, would trade
this against matcher speed.

### 10. GC

5–6% of CPU time, 67.6 GB consed in the full suite. Top allocators
(`:alloc` run): `PCTIMES` 7.1%, `FPQUOTIENT` 7.1%, SBCL compiler 6.8%,
`PCPLUS` 4.6%, `FPROUND` 4.4%, `FPDIFFERENCE` 3.8%, `TIMESIN` 3.0%.
`HYPERGEOMETRIC-BY-SERIES` is 22.7% inclusive. Bignum results dominate.
Prototype `prototypes/gc.lisp` raises `bytes-consed-between-gcs` from 51 to
205 MB. GC time drops from 8.2 to 2.9 s in the full suite, but total CPU time
moves by no more than the noise (−1.9 ± 2.4 s full, −1.4 ± 0.5 s core,
−0.2 ± 0.3 s on the integrate files), so the GC time saved is apparently paid
back elsewhere. Cutting allocation itself is the better lever.

### Smaller items

- `ORDERPOINTER`/`PRENUMBER` (2.1% full, 2.6% core): a fresh, readably named
  `GENSYM` per variable on each `ratrep*`, then `SET` on every genvar. Using
  `MAKE-SYMBOL` and writing the global value directly saved nothing measurable
  (≤0.3%) and changed `rtest15` #49 (genvar names matter for ordering
  somewhere), so not worth it as is. (−0.2 s in an early concurrent
  screening, not re-measured.)
- `FPROUND` binds `*print-base*` and `*print-radix*` on every call though only
  the decimal branch needs them.
- `NORMALIZED-MODULUS` (1.4% full) is generic `mod` on bignums in modular
  factoring.
- `ALIKE1`/`ALIKE` (5–7%): already tuned. The volume comes from `ORDLIST`
  (term merging in `PLUSIN`) and `ASSOL` (item 7).

### Not hot

Reading test files (`MREAD`) is 0.5–0.6% and result comparison
(`BATCH-EQUAL-CHECK`) 0.3%, so the harness itself does not distort the
picture.

## Round 2: master with the first three patches

Master at `eec2fa780`, which has the `sign` shortcut for linear sums
(`4fa98b94a`), the cheaper `def-simplifier` (`3d4c7d2af`) and the new bigfloat
square root (`eec2fa780`). Same build and method as above: one unprofiled
warm-up, two `:cpu` runs per suite at 4 ms, one `:alloc` run of the full
suite, all reporting `No unexpected errors`. Data in `data/round2/`.

|                                     | core          | full            |
|-------------------------------------|---------------|-----------------|
| CPU time (profiled, 2 runs)         | 62.5–63.2 s   | 116.4–117.2 s   |
| CPU time (unprofiled, A/B baseline) | 60.8 s        | 114.6 s         |
| GC time                             | 6.2%          | 7.6%            |
| bytes consed                        | 29.0 GB       | 59.0 GB         |

Round 1 profiled on a slower host, so the comparison uses shares
(`rounds.py`):

| subsystem (frame on stack)              | core r1 | core r2 | full r1 | full r2 |
|-----------------------------------------|---------|---------|---------|---------|
| `limit`                                 | 32.3%   | 29.0%   | 19.8%   | 16.2%   |
| `sign` (`SIGN1`)                        | 24.6%   | 17.6%   | 20.1%   | 14.2%   |
| `MEQP` (equality via `csign`)           | 12.7%   | 10.2%   | 10.0%   |  7.9%   |
| `ratsimp` (`FULLRATSIMP`/`SRATSIMP`)    | 21.2%   | 17.0%   | 20.1%   | 16.5%   |
| `integrate`                             | 17.8%   | 20.0%   | 14.3%   | 15.0%   |
| `factor`                                | 15.5%   |  8.2%   | 11.2%   |  5.5%   |
| bigfloat arithmetic (`FP*`)             |  8.7%   |  4.2%   |  5.8%   |  3.0%   |
| `taylor`                                |  6.6%   |  7.0%   |  4.1%   |  4.1%   |
| SBCL compiler at test time              |  2.6%   |  3.5%   |  5.4%   |  6.5%   |

`factor` and `FP*` halved, as expected. Everything else now has a larger share
of a smaller total. Top test files: core `rtest_integrate` 22.0%,
`rtest_limit_extra` 12.0%, `rtest_limit_gruntz` 8.2%, `rtest_limit` 6.5%,
`rtest_trig` 5.8%. Full `rtest_integrate` 11.8%, `rtest_abs_integrate` 10.4%,
`rtest_matrixexp` 6.9%, `rtest_limit_extra` 6.3%, `rtest_limit_gruntz` 4.4%,
`rtest_cholesky` 4.3%.

### Measured gains

Prototypes are in `prototypes/` as before. Targeted runs, three rounds after a
discarded warm-up:

| prototype     | item | test files (baseline)                                          | change                |
|---------------|------|----------------------------------------------------------------|-----------------------|
| `exptofbase`  | 1    | `rtest_limit_extra`, `rtest_trig`, `rtest_limit`, `rtest16`, `rtest_integrate` (32.1 s) | −2.6 ± 0.1 s (−8.1%)  |
| + `mabsrewrite` | 2  | same                                                           | −3.4 ± 0.3 s (−10.6%) |
| + `signlog`   | 3    | same                                                           | −4.3 ± 0.2 s (−13.5%) |
| `limhash`     | 4    | `rtest_limit_extra`, `rtest_limit_gruntz`, `rtest_limit` (17.2 s) | −1.5 ± 0.3 s (−8.9%) |
| `scalarclass` | 5    | `rtest_cholesky`, `rtest_matrixexp` (12.1 s)                    | −1.9 ± 0.3 s (−15.9%) |
| `modfix`      | –    | the limit files / `rtest_cholesky`, `rtest_matrixexp`           | −0.0 ± 0.3 s / −0.3 ± 0.4 s |

Whole suites, three rounds:

| config                              | full, base 114.6 s     | core, base 60.8 s      |
|-------------------------------------|------------------------|------------------------|
| `r2`: items 1–5                     | −10.5 ± 0.3 s (−9.2%)  | −6.2 ± 0.4 s (−10.1%)  |

All runs pass. The patch in `handover-speedups-round2.md` (items 1–5, item 2
as `mabscsign`), built in and timed against the same build with the old
definitions loaded: core −6.1 ± 0.2 s (−10.9%), full −11.5 ± 0.8 s (−10.3%).
`ab-round2.txt` in `data/round2/` has every run.

### All patches together

Three separate builds timed against each other (`abmulti.sh`, one discarded
warm-up per build, three rotated rounds, pinned): master `eec2fa780` with
the three round-1 commits reverted, master as is, and master with the round-2
handover applied. All runs pass.

| build                        | core           | full            |
|------------------------------|----------------|-----------------|
| before both rounds           | 68.0 s         | 120.0 s         |
| round 1 (on master)          | 58.1 s (−14.7%) | 108.1 s (−9.9%) |
| rounds 1 and 2               | 51.1 s (−24.9%) | 98.0 s (−18.3%) |

Round 2 alone: core −7.0 ± 0.2 s (−12.0%), full −10.1 ± 0.4 s (−9.3%).
Runs in `data/round2/ab-overall.txt`.

### 1. `SIGNDIFF-SPECIAL` calls `EXPT-OF-BASE` before the cheap tests

`SIGNDIFF-SPECIAL` is 8.0% of the core suite, and 3.7% is `EXPT-OF-BASE`,
which decides via `MEQP` (`csign` of a ratsimped difference) whether one
expression is a power of another. Two rules call it before the sign tests that
decide whether its answer matters:

- `Q^R - S`: the guard `(not (expt-of-base xrhs (cadr xlhs)))` comes before
  `Q > 0` and `S > 0`.
- `Q^m - Q^n`: both exponents `m` and `n` are looked up (two `MEQP`s) before
  `Q > 0` and the sign of `Q - 1`. Over `rtest_limit_extra`, `rtest_trig`,
  `rtest_limit`, `rtest16` and `rtest_integrate` this rule ran 89,800 times.
  `Q` was not known positive in 82,800 of them, and the rule could apply in
  only 2,100.

Prototype `prototypes/exptofbase.lisp` moves the sign tests first. All the
reordered tests are side-effect-free predicates, so the result is the same,
and the recursion guard still comes before the recursive `sign*`.
`EXPT-OF-BASE` calls in those five files drop from 200,384 to 7,823 (2.82 to
0.15 s).

### 2. `SIGN-MABS`: `MNQP` repeats a sign computation that just failed

`sign(abs(e))` first takes the sign of `e`. If that is `pnz`, `mnqp(0, e)`
asks whether `e` can be zero. Over the full suite that happened 132,116
times and never succeeded, yet these calls are 3.9% of the core and 2.4% of
the full suite's samples. Within that `MEQP`, `csign` takes 64%,
`sratsimp` 23% and the fact database 2%. In 98% of the calls (119,295 of
121,614, on four files) `sratsimp(-e)` is `-e` again, so the `csign` only
repeats the computation that just returned `pnz`.

A plain skip loses real cases: under `assume(notequal(x+y,0))`,
`sign(abs(x+y))` is `pos` only through the fact database, and
`sign(abs((x+1)^2-x^2-2*x+y^2))` is `pos` only because ratsimp turns the
argument into `y^2+1`. `prototypes/mabsrewrite.lisp` keeps both: for `pnz`
it runs the cheap part of `MEQP` (`PROVABLY-NONZERO-P` and `DCOMPARE`) and
calls `MNQP` only when ratsimp and the equality facts rewrite `-e`.
`prototypes/mabscsign.lisp`, the version for a patch, keeps all of `MEQP`
instead: `MEQP` and `MNQP` take an optional `csign`, and with it false `MEQP`
skips only its final `csign` when ratsimp changes nothing. Both are equally
fast (−4.1 ± 0.3 and −4.2 ± 0.3 s with items 1 and 3, `ab-round2.txt`).
Neither is strictly equivalent: when the rewrite changes nothing, `$csign`
could still answer differently than the `sign` that just failed, since it
rebinds `limitp` and `factored`. That never occurred in either suite.

### 3. `SIGN-LOG` computes each comparison twice

For a positive argument, `SIGN-LOG` tries `mgrp(1, arg)`, `meqp(arg, 1)`,
`mgqp(1, arg)`, `mgrp(arg, 1)`, `mgqp(arg, 1)` and `mnqp(arg, 1)` in turn.
That is `csign(1 - arg)`, `meqp(arg, 1)` and `csign(arg - 1)` twice each.
`prototypes/signlog.lisp` computes each once, under the same condition as its
first use in the cascade, and reuses it, so the result is the same.

Simpler still is `prototypes/signlog2.lisp`. For `arg > 0`, `log(arg)` has the
sign of `arg - 1`, and the sign of `1 - arg` is just its flip. So one `csign`
settles `pos`, `neg`, `zero` and `pn`, and only for `pz`, `nz` and `pnz` can
`MEQP` add something, by proving `arg = 1`. Over the full suite 3,593 calls
had `arg >= 0`. The sign of `1 - arg` was `pnz` 2,616 times, `neg` 908, `pos`
67, `nz` 2 and `zero` never. The two signs were always flips of each other,
and the result always matched the cascade's. The instrumented run puts the
saving over `signlog` at 0.6 s on the full suite. On the five files above it
is below the noise, -0.0 ± 0.3 s.

`MEQP` never decided anything in the suite. In all 2,618 calls where the sign
of `arg - 1` was open, it answered unknown too, at about 0.6 s in total. It is
not redundant though: before its `csign` it substitutes every `equal()` fact
and ratsimps, which `sign` does not. Under
`assume(x > 0, y > 0, z > 0, equal(x, y + z))`, `sign(log(x/(y + z)))` is
`zero` with it and `pnz` without.

`prototypes/signlog4.lisp` (load after `mabscsign.lisp`) also passes item 2's
new `MEQP` argument as false when the sign of `arg - 1` is `pz`, `nz` or
`pnz`. `MEQP` then repeats that sign only when ratsimp or the equality facts
rewrite `arg - 1`, so the case above still gives `zero`. In the full suite
that skipped the repeat in 1,210 of the 2,618 calls and cut their `MEQP` time
from 0.63 s to 0.26 s, with the same results.

### 4. Limit cache lookups (round 1, item 7)

`GETLIMVAL` and `PUTLIMVAL` cost 3.0% of the core suite in `ALIKE1`, `ALIKE`
and `ASSOL`, a linear search of `limit-answers` with `ALIKE1` on every key.
`prototypes/limhash.lisp` stores a hash code with each entry, built from
operators, symbols, integers and strings four levels deep and ignoring header
flags, so `ALIKE1`-equal keys always get equal codes. `ALIKE1` then runs only
on entries whose code matches.

### 5. `SCALARCLASS` re-walks constant subtrees (round 1, item 8)

Now 2.4% of the full suite, nearly all from `rtest_cholesky` problems 35
and 36 (block matrices in `noncommutingring`), 3.6 s of the file's 3.8 s.
`CONSTTERMP` runs `$constantp` over a term and then `SCALARCLASS` on it,
which runs `CONSTTERMP` on each argument again, so every level walks its
whole subtree once more. Problem 36 makes 36.6M `$constantp` calls for 3.0M
`SCALARCLASS` calls. In `prototypes/scalarclass.lisp` `SCALARCLASS` and
`SCALARCLASS-LIST` take an optional flag saying that `$constantp` of the
expression is already known to be true. Then the arguments are constant too
(that is how `$constantp` is defined), and the walk is not repeated. Same
results, 5.4M `$constantp` calls.

### 6. Share Lisp files are compiled at every load (round 1, item 5)

The SBCL compiler now takes 6.5% of the full suite, about 7 s per run.
`load()` of share `.lisp` files accounts for 3.4–3.7% (`LOADFILE` on the
stack), rule definitions (`META-FSET`) about 0.8%, `translate` 0.1%, the rest
are stacks too deep to tell. Loading those files from fasls compiled once
would save most of the 4 s, but a cache bolted onto `LOADFILE` changes what
the files mean, because loading source compiles and runs one form at a time:

- Compiling the file first (`COMPILE-FILE`, then load the fasl) breaks
  `fourier_elim`: `fourier_elim.lisp` loads `to_poly.lisp`, which defines the
  `OPAPPLY` macro it uses, only at load time, so the fasl calls `OPAPPLY` as
  a function.
- Compiling after the load (`prototypes/faslcache.lisp`) gets that right, but
  then declarations later in a file apply to forms before them. With it, the
  full suite fails in `rtest_itensor` and exhausts the heap in itensor's
  `DELTA`.

So this needs work per package: make the heavy ones compile cleanly as
files, as the `.system` packages (`draw`, `lapack`, ...) already do, and
compile them into the objdir. By load time in the full suite the candidates
are `fourier_elim`, `stringproc`, `to_poly_solve`, `grobner`, `numdistrib`,
`bitwise`, `itensor` and `pslq` (`data/share-loads.txt`).

### Smaller items and non-starters

- `NORMALIZED-MODULUS` (1.4% full) is generic `mod`. A fixnum fast path
  (`prototypes/modfix.lisp`) measured nothing beyond the noise.
- `MEQP` ratsimps the difference, then `MEQP-BY-CSIGN` ratsimps it again
  (1.25% core, 0.8% full). Over the full suite the second pass changed the
  expression in 12,043 of 296,254 calls, so it cannot simply go.
- Binding (`MBIND-DOIT` + `MUNBIND`, 4.4% full) is unchanged since round 1.
  `$values` holds about ten entries, so the remaining cost is volume. The
  round-1 `mbind` prototype still applies.
- `GET3` is 8.8% of full-suite samples (self). Plists are already ordered
  for the simplifier (`OPTIMIZED-PLIST` in `src/init-cl.lisp`), so the walks
  are short. `MOPP1`'s share (11% of `GET3`) goes with item 5.
- `HYPERGEO21-FLOAT` (`share/orthopoly`, 0.9% full) is a float loop in
  generic arithmetic.
- `PCTIMES`/`PCPLUS` lead allocation (8% and 5%, mostly `rtest_extensions`
  and `rtest15`): bignum polynomial arithmetic, nothing local to fix.

## Files

- `prof.lisp`: the harness (`prof-start`, `prof-finish`): per-test-file
  wrappers, attribution, folded stacks.
- `runprof.sh NAME MODE INTERVAL 'run_testsuite args' [t|nil]`: one profiled
  run into `profiling/out/`.
- `stk.py FILES CMD ...`: queries on folded stacks (`tables`, `incl`,
  `callers`, `callees`, `tree`, `files`, `leaves`, `attr`), reads `.gz`.
- `abrun.sh` times one run with `prototypes/*.lisp` loaded, `abseq.sh` runs
  the interleaved rounds, `abstat.py` summarizes them. `abmulti.sh` does the
  same for separate builds.
- `instrumentation/`: the counting wrappers behind the numbers above
  (`sfstat.lisp`, `sfdebug.lisp`, `simpstat.lisp`, `loadlog.lisp`,
  `dolcount.lisp`, `lenlog.lisp`, `fpsqrt-check.lisp`, `fpsqrt-exact.lisp`).
- `data/`: per-run summaries, sb-sprof flat reports, attribution and per-file
  tables, folded stacks (`*.folded.gz`), combined tables
  (`tables-core.txt`, `tables-full.txt`), share load times, SIGNFACTOR
  statistics, A/B runs and their summaries (`ab-results.txt`).
- Round 2: `data/round2/` (same files, plus `ab-round2.txt`), `rounds.py`
  (subsystem shares of both rounds), prototypes `exptofbase`, `mabsrewrite`,
  `mabscsign`, `signlog`, `limhash`, `scalarclass`, `modfix`, `faslcache`, and
  instrumentation `eobcount.lisp`, `mabscount.lisp`, `mabsalike.lisp`,
  `scalarclasscount.lisp` (with `scalarclass-setup.mac`).

Reproduce: build, run the full suite once (compiles the `.system` share
packages), then

    profiling/runprof.sh core-cpu :cpu 0.004 ''
    profiling/runprof.sh full-cpu :cpu 0.004 'share_tests=true'
    profiling/stk.py profiling/out/full-cpu.folded tables 40

A/B timing of prototypes, for example:

    PIN=3 profiling/abseq.sh full 3 'share_tests=true' \
      base= signfactor=signfactor best=signfactor+fpsqrt > full.txt
    profiling/abstat.py full.txt

Flame graph with Brendan Gregg's `flamegraph.pl`:

    zcat profiling/data/full-cpu-1.folded.gz | sed 's/^.*\(|TESTFILE \)/\1/' |
      flamegraph.pl --countname samples > full.svg
