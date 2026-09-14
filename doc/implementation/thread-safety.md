# Shared mutable state in Maxima

An inventory of the state two computations would share if Maxima ever ran
them at the same time, checked against the source rather than collected
from memory.

It is worth having whether or not Maxima ever grows threads. Most of what
is below is also the answer to "why did that global change under me", and
several entries are latent bugs in the single-threaded code -- a cache
that can exceed its own documented limit, an unwinding loop that depends
on a variable nobody thinks of as shared.

## How to read this

Every claim is marked **measured** or **read**. "Read" means it was
worked out from the source and not run; the two disagree often enough in
this codebase that the distinction is worth the words. Rates like "2 of
200" come from running the case that many times on four cores.

Locations are file plus construct name, never line numbers.

Entries are grouped by the **kind** of state, because the kind decides
what could fix it. A variable that merely holds the state of one
computation can be bound per thread and needs nothing else; a counter on
a property list cannot be bound at all.

## 1. Specials holding the state of one computation

The easy case, and the one the existing groundwork addresses. These hold
something belonging to a single evaluation, so a binding at the point a
thread starts makes every assignment below it land in that thread's own
binding, and no caller or callee has to change.

| state | where | note |
|---|---|---|
| `SIGN`, `MINUS`, `ODDS`, `EVENS` | `src/compar.lisp` | one answer in four variables, set together |
| `WIDTH`, `HEIGHT`, `DEPTH` | `src/displa.lisp` | box dimensions |
| `VARLIST`, `GENVAR`, `VLIST` | `src/rat3*.lisp` | CRE's variables and their ordering |
| `TSTACK`, `*LOCAL-SIGNS*`, `$MULTIPLICITIES`, `$%RNUM_LIST`, `$ERROR`, `$ERROR_SYMS`, `$LINENUM`, `$GENSUMNUM`, `$INTEGRATION_CONSTANT_COUNTER` | various | state of one line of computation |

`WITH-THREAD-LOCAL-ENVIRONMENT` (`src/suprv1.lisp`) binds these.
**Measured** by the groundwork on a `SIGN`-shaped protocol raced between
two threads: 1797 wrong answers in 4000 unbound, 0 bound.

## 2. Specials that are one half of a pair

The trap in the first category. Some of these variables are bookkeeping
for state that lives somewhere a binding cannot reach, and binding one
without the other is worse than leaving both alone.

**`$FPPREC` and the bigfloat constants.** `$FPPREC` carries an `ASSIGN`
property of `FPPREC1`, so an ordinary `fpprec: 30` rewrites the working
precision `FPPREC` and rebuilds `*BIGFLOATONE*`, `*BIGFLOATZERO*`,
`*BFHALF*` and `*BFMHALF*` from it, globally, in one go. Binding a subset
leaves a thread whose precision disagrees with its own constants. (read)

**`CURRENT` and the `CMARK` counts.** `CURRENT` (`src/db.lisp`) records
which context the fact database has marked. `CONTEXTMARK` does nothing
when it already equals `CONTEXT`, so sharing it is why a parallel region
nested inside a parallel body cannot see the facts of the body that
started it. But binding it is worse: the other half of the pair is the
`CMARK` counts on the context symbols' plists (sec. 6), which no binding
can reach, so a thread with its own `CURRENT` unmarks its caller's chain,
marks its own, and then loses the binding on the way out while the counts
it changed stay changed. **Measured**: binding `CURRENT` alone took a
plain inherited `assume()` from 0 wrong in 200 to **200 wrong in 200**.

> The rule this gives: before binding anything here, ask what else moves
> when it moves. The pair goes together or not at all.

## 3. Symbol value cells written through a generic setter

Maxima binds a user variable by **saving the symbol's value, `MSET`ting
it, and putting the old value back** -- `MBIND-DOIT` and `MUNBIND` in
`src/mlisp.lisp`. That writes the one global value cell every thread
shares, and because the write goes through `MSET` rather than a `SETQ`
naming the variable, a cross-reference search for assignments finds
nothing at all.

**Measured**: a parallel `makelist` whose body was no more than `i^2`
returned a wrong list **2 times in 200**, an element having been computed
with another element's index. Giving each runner its own binding of the
loop variable: 0 in 200.

This is also the shape the observation half of
`lisp-utils/thread-safety-survey.lisp` is blind to: a save-and-restore
leaves no net change, so watching value cells over a `run_testsuite()`
cannot see it either. Both halves of that tool miss this, and it sits in
the evaluator's most central mechanism.

## 4. Shared stacks

`BINDLIST` and `MSPECLIST` (`src/mlisp.lisp`) are the stacks `MBIND`
pushes saved values onto; `LOCLIST` (`src/globals.lisp`) is `MLOCAL`'s.

They are not merely accounting. `ERRCATCH` (`src/errset.lisp`) saves
`(cons bindlist loclist)`, and `ERRLFUN1` (`src/suprv1.lisp`) unwinds by
calling `MUNLOCAL` until `LOCLIST` is `EQ` to the cons it saved. Shared
between threads, that cons is no longer anywhere in the unwinding
thread's chain, so the loop pops an already-empty `LOCLIST` for ever.

**Measured**: an error raised in one element of a parallel `makelist`
left the whole run spinning at full CPU with its workers already gone, on
about **half** of the runs of the error test and **one full suite run in
five**. With `LOCLIST` bound per runner: 0 of 30, and 0 of 12.

Note `MUNLOCAL` also pops `MPROPLIST` and `FACTLIST`, which `MLOCAL`
pushes alongside `LOCLIST`. What a `local()` inside a parallel body does
to those has not been measured. (read)

## 5. Lists mutated by read, modify, write

**`$CONTEXTS`.** `$SUPCONTEXT` (`src/compar.lisp`) registers a new
context with `(setq $contexts (mcons name $contexts))` -- a read, a cons
and a write of one shared list. Two runners doing it at once lose one of
the two names, and the runner whose name went missing then dies with
`supcontext: no such context ctxt<n>`.

**Measured**: `parallel_makelist(integrate(x^i, x), i, 1, 8)` failed
**30 times out of 30**, and a scratch context was left behind in the
user-visible `contexts` list. `integrate()` reaches this constantly
through `WITH-NEW-CONTEXT` (`src/maxmac.lisp`), which makes a gensym
context, works in it and kills it again. With the context variables bound
per runner: 0 of 30, nothing left behind.

**`$PROPS`, `$VALUES`, `$FUNCTIONS`, `$RULES`, `$ARRAYS`, `$LABELS`,
`$STRUCTURES`.** Same family. `ADD2LNC` extends `$PROPS` with
`(nconc llist (ncons item))`, mutating the list in place and never
assigning the variable, so a cross-reference search sees nothing here
either. (read)

## 6. Counters on property lists

Which facts are visible is decided by `CONTEXTMARK` (`src/db.lisp`),
which keeps a **count on each context symbol's plist** and walks the
chain incrementing and decrementing it:

```lisp
(defun cmark (con)
  (let ((cm (zl-get con 'cmark)))
    (putprop con (if cm (1+ cm) 1) 'cmark)
    (mapc #'cmark (zl-get con 'subc))))
```

A read, an add and a write, on state every thread shares. Concurrent
runners lose each other's updates and the count on a context holding real
assumptions drifts to zero, after which its facts are invisible.

**Measured**: with each runner given its own context (an attempt at
scoping a body's facts to the body), a plain inherited `assume()` went
from right every time to **wrong 157 times in 200**, getting worse the
longer the session ran, against **0 in 200** on the serial path -- which
creates and kills exactly the same contexts. The scoping was therefore
sound and concurrency alone broke it. It is written down and switched off
in `src/parallel.lisp`.

> **A lock here is not enough, and this is the entry most likely to be
> got wrong.** Serialising the counter walk stops updates being lost and
> is still incorrect, because the count is a count: two runners' chains
> end up marked at the same time and each can then see the other's facts.
> Marking has to become per thread, which means these counts have to stop
> living on shared plists.

## 7. The fact database

The assumptions themselves do not live in variables. They live on
**symbol plists** -- `src/db.lisp` writes them with `putprop`, and a
symbol's facts hang off its `data` property. There is no variable to
bind, so no dynamic binding can make the database per thread; it wants
either a lock or a real per-thread store.

The interning tables in front of it are shared in the ordinary way as
well: `DINTERN` (`src/db.lisp`) extends `DOBJECTS` with
`(setq dobjects (cons (dbnode x) dobjects))`, and numbers go into
`*NOBJECTS*` the same way -- read, cons, write, as in sec. 5. So two
threads mentioning an expression the database has not seen before race
before either of them has asserted anything. (read)

**Measured**, with the context variables bound per runner and nothing
else: a body's own assumption was invisible to its own `sign()`
**2 times in 200** with threads, **0 times in 200** on the serial path.

Two lifetimes share the one store, which complicates any scheme: `assume`
facts survive between inputs, while `asksign` facts are cleared between
them by `CLEARSIGN` (defined in `src/compar.lisp`, called from
`src/suprv1.lisp`).

## 8. In-place mutation a cross-reference search cannot see

**`LINEARRAY`** (`src/displa.lisp`) is the clearest example in the tree:
it is **never assigned anywhere**. It is `defvar`'d once and thereafter
mutated with `(setf (aref linearray i) ...)` and `(fill linearray nil)`.
A search for assignments to it returns nothing, and it appears in the
survey's classification only because somebody read the code. It is
`DISPLA`'s layout scratch, so two threads displaying at once corrupt each
other's output before a character reaches any stream -- and a lock around
the *writing* would not help, since the corruption is in the buffer.

For a user-settable option variable, **a static count of zero is not
evidence of anything**.

## 9. Global hash tables

Most of Maxima's global hash tables are filled once and read thereafter
(`*FLONUM-OP*`, `*BIG-FLOAT-OP*`, `*OPR-TABLE*`, `*RUNNING-ERROR-OP*`,
`*COLOR-TABLE*`, the `intl` tables, `*BUILTIN-SYMBOL-PROPS*` and
`*BUILTIN-SYMBOL-VALUES*`). Those are not a threading problem.

The ones written **at run time** are:

| table | where | written by |
|---|---|---|
| `*LAMBDA-EXPR-FUNS*` | `src/mlisp.lisp` | `LAMBDA-EXPR-FUN`, on every miss |
| `*DIRECTORY-CACHE*` | `src/mload.lisp` | directory lookups |
| `*TEMP-FILES-LIST*` | `src/plot.lisp` | plotting |

**`*LAMBDA-EXPR-FUNS*` is the interesting one** and is worth fixing
regardless of threads. It memoises compiled functions for Lisp lambda
expressions applied by `MAPPLY1`. It is a plain, unsynchronised hash
table, and its eviction branch runs `WITH-HASH-TABLE-ITERATOR`, `RANDOM`
on one shared random state, and `REMHASH` -- a combination the standard
does not define under concurrent modification.

**Measured**, four threads each applying 400 distinct expressions:

| run | worker threads that died | final size (limit 128) |
|---|---|---|
| 1 | 4 of 4 | 14 |
| 2 | 2 of 4 | **129** |
| 3 | 1 of 4 | **135** |

The deaths are SBCL's own internal assertion,
`failed AVER: (= SB-IMPL::HWM (HASH-TABLE-SIZE TABLE))` -- the table's
internal structure corrupted, not merely a wrong answer. And the cache
exceeded the limit its own docstring says it must not grow past.

**Reachability**: `MAPPLY1` uses this branch only for a **Lisp** lambda;
Maxima's own `lambda([x], ...)` is `((lambda) ...)` and goes to
`MLAMBDA` instead. So this is a real defect on a path ordinary Maxima
code does not currently reach -- latent rather than live. The fix is one
keyword (`:synchronized t`, which SBCL and CCL both support) or a lock
around the three operations.

## 10. Streams

A new thread inherits no dynamic bindings, only global values, so a
thread that reads `*STANDARD-OUTPUT*` for itself gets the session-wide
stream and not whatever its caller had bound.

This is not theoretical: `RUN_TESTSUITE` rebinds `*STANDARD-OUTPUT*`
around each problem to capture what it prints (`TEST-BATCH`,
`src/mload.lisp`). **Measured**: a message printed by a worker walked
past the harness into the log while the same message printed by the
calling thread was caught, so the suite's own output changed from run to
run according to which runner took the failing element -- found by
diffing three whole-suite runs against each other rather than comparing
their headline results. Capturing the streams in the thread that starts
the workers: 0 of 8 runs.

Also per-lisp: `read()` takes `*STANDARD-INPUT*` under SBCL and CMUCL and
`*QUERY-IO*` elsewhere (the `#+(or sbcl cmu)` in `src/macsys.lisp`), so a
worker must bind both. (measured by the groundwork)

## What this adds up to

Three of these can be fixed by binding, and are: the per-computation
specials (sec. 1), the stacks (sec. 4), the streams (sec. 10). One is
fixed by binding but only if the whole pair moves (sec. 2, sec. 5).

Two cannot be fixed by binding at all, because the state is not in a
variable: the `CMARK` counts (sec. 6) and the fact database itself
(sec. 7). Those two are the real decision, and the ordering between them
is forced -- the counts have to become per-thread before any scheme for
the database can be evaluated, because until then every experiment on the
database is measuring the counts instead.

One is a plain bug worth fixing on its own account (sec. 9).

## Method, and two things that cost time

**Every measurement needs a control.** A first attempt to observe what a
`makelist` body disturbs reported 0 of 958 variables moved, which read as
a clean result. It was not: every body had errored on a bad parse call and
the workload never ran. What exposed it was asserting that `$LINENUM`
must move, since it moves for any input that runs at all.

The control that separated design from implementation throughout this
document is **the serial path**: running the same code with the thread
limit set to 1 exercises identical machinery without concurrency. Where
serial is clean and threaded is not, the design is sound and the sharing
is the problem -- which is how sec. 6 was diagnosed.

**Say whether a claim was measured or read.** Two things in this document
were believed, written down, and then turned out to be wrong when run:
that the CRE `GENVAR` renumbering was corrupting parallel rational
arithmetic (it was the loop variable, sec. 3), and that scoping a body's
facts to its own context would work (sec. 6).
