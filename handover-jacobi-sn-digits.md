# Fix jacobi_sn() and friends losing digits

## 1. Summary

**`jacobi_sn`, `jacobi_cn`, `jacobi_dn` and `jacobi_am` no longer lose up to
12 digits for arguments with a larger imaginary part, or for real
arguments with `m > 1`.** The descending Landen transformation computed
`1 - sqrt(1 - m)` by subtraction, which loses digits when `m` is small,
and stopped as soon as `abs(m)` was below the machine epsilon, which is
too early when the argument has a large imaginary part. Both are fixed.
`jacobi_cn` for `m` near 1 still loses about 5 digits, a separate issue
this does not change.

## 2. Reproducer/Demo

Old behavior (HEAD):

```
(%i1) jacobi_sn(20.0,2.5)
(%o1)                         0.2282499166210549
(%i2) jacobi_sn(20.0*%i,0.75)
(%o2)                       - 0.2325375293373911 %i
(%i3) jacobi_sn(20.0*%i,1.75+%i)
(%o3)              0.827993704767369 - 0.8735275803124387 %i
(%i4) jacobi_dn(20.0*%i,0.75)
(%o4)                         1.0200761132938823
(%i5) fpprec:32
(%i6) jacobi_sn(2.0b1*%i,1.75b0+%i)
(%o6) 8.2802449307802841784308302249899b-1
                                      - 8.7382166197456268660630255079068b-1 %i
```

New behavior:

```
(%i1) jacobi_sn(20.0,2.5)
(%o1)                         0.2282498061811354
(%i2) jacobi_sn(20.0*%i,0.75)
(%o2)                      - 0.23257157765273614 %i
(%i3) jacobi_sn(20.0*%i,1.75+%i)
(%o3)             0.8280244930780285 - 0.8738216619745593 %i
(%i4) jacobi_dn(20.0*%i,0.75)
(%o4)                          1.020081934968418
(%i5) fpprec:32
(%i6) jacobi_sn(2.0b1*%i,1.75b0+%i)
(%o6) 8.2802449307802841784936596033771b-1
                                      - 8.7382166197456268660821131437645b-1 %i
```

Correct values, computed with mpmath at 40 digits and confirmed by Maxima
at `fpprec : 90`:

| expression | correct value |
|------------|---------------|
| `jacobi_sn(20.0, 2.5)` | `0.2282498061811287` |
| `jacobi_sn(20.0*%i, 0.75)` | `-0.2325715776527406*%i` |
| `jacobi_sn(20.0*%i, 1.75+%i)` | `0.8280244930780284 - 0.8738216619745627*%i` |
| `jacobi_dn(20.0*%i, 0.75)` | `1.020081934968419` |
| `jacobi_sn(20b0*%i, 1.75b0+%i)` | `8.28024493078028417849365960338059b-1 - 8.73821661974562686608211314376421b-1*%i` |

Internals: near the end of the recursion `1 - sqrt(1 - m)` cancels, and
the resulting relative error of `root-mu` goes straight into the result
when `sn` is large there, as it is for arguments with a large imaginary
part. Stopping at `abs(m) < epsilon` neglects a term of relative size
`m*exp(2*abs(imagpart(u)))`. `DN` computes the same root, and `CN` uses
`DN`. `jacobi_am` uses `asin(jacobi_sn(...))` for complex arguments or
`abs(m) > 1`.

Over 120 arguments (the tests 240 to 250 and random complex `u` and `m`),
compared with mpmath, the largest error of `jacobi_sn` drops from 2.2e12
to 3.4e3 units in the last place with doubles and from 4.0e14 to 600
with `fpprec : 32`. The median drops from 443 to 13 and from 951 to 17
units. `jacobi_dn` and `jacobi_cn` improve the same way, except
`jacobi_cn` at `m = 0.99`, which loses about 5 digits before and after.
With the fix, `jacobi_sn` also no longer depends on how the bigfloat
square root rounds.

## 3. Bug report

**Title:**
```
jacobi_sn and jacobi_am lose many digits for larger arguments
```

**Body:**
````markdown
`jacobi_sn` returns wrong digits for moderately large arguments, with floats and with bigfloats:

```
(%i1) display2d:false$
(%i2) jacobi_sn(20.0, 2.5);
(%o2) 0.2282499166210549
(%i3) jacobi_sn(20.0*%i, 0.75);
(%o3) -0.2325375293373911*%i
(%i4) jacobi_sn(20.0*%i, 1.75+%i);
(%o4) 0.827993704767369-0.8735275803124387*%i
```

The correct values are `0.2282498061811287`, `-0.2325715776527406*%i` and `0.8280244930780284-0.8738216619745627*%i` (computed with mpmath, and Maxima agrees at `fpprec : 90`). So the first result is wrong from the 7th digit on, and the others from the 4th or 5th. `sin(jacobi_am(z, m))` gives the same wrong values, and `jacobi_dn` and `jacobi_cn` are affected too, for example `jacobi_dn(20.0*%i, 0.75)` gives `1.0200761132938823` instead of `1.020081934968419`. With bigfloats at `fpprec : 32`, `jacobi_sn(20b0*%i, 1.75b0+%i)` has only about 20 correct digits. The error grows quickly with `abs(z)`.

The tests 240 to 250 in `rtest_elliptic.mac` do not notice, because they only check that `sin(jacobi_am(z, m))` equals `jacobi_sn(z, m)`.
````

## 4. Code patching instructions

### `src/ellipt.lisp`, `ASCENDING-TRANSFORM`, compute sqrt(mu1) without cancellation

**Replace** this block

```
  ;; sqrt(mu1).
  (let* ((root-m (sqrt m))
	 (mu (/ (* 4 root-m)
		(expt (1+ root-m) 2)))
	 (root-mu1 (/ (- 1 root-m) (+ 1 root-m)))
```

with

```
  ;; sqrt(mu1).  We compute it as (1-m)/(1+sqrt(m))^2, which is the
  ;; same without the cancellation in 1-sqrt(m) when m is near 1.
  (let* ((root-m (sqrt m))
	 (mu (/ (* 4 root-m)
		(expt (1+ root-m) 2)))
	 (root-mu1 (/ (- 1 m) (expt (+ 1 root-m) 2)))
```

### `src/ellipt.lisp`, `DESCENDING-TRANSFORM`, compute sqrt(mu) without cancellation

**Replace** this block

```
  ;; sqrt(mu) loses information when m or m1 is complex.
  (let* ((root-m1 (sqrt (- 1 m)))
	 (root-mu (/ (- 1 root-m1) (+ 1 root-m1)))
```

with

```
  ;; sqrt(mu) loses information when m or m1 is complex.  We compute
  ;; sqrt(mu) as m/(1+sqrt(m1))^2, which is the same without the
  ;; cancellation in 1-sqrt(m1) when m is small.
  (let* ((root-m1 (sqrt (- 1 m)))
	 (root-mu (/ m (expt (+ 1 root-m1) 2)))
```

### `src/ellipt.lisp`, `ELLIPTIC-SN-DESCENDING`, stop the recursion later for complex arguments

**Replace** this block

```
	((< (abs m) (epsilon u))
	 ;; A&S 16.6.1
	 (sin u))
```

with

```
	((or (zerop m)
	     ;; sn(u,m) = sin(u) + O(m*sin(u)*cos(u)^2), A&S 16.13.1, and
	     ;; cos(u)^2 grows like exp(2*abs(imagpart(u))).
	     (< (+ (log (abs m)) (* 2 (abs (imagpart u))))
		(log (epsilon u))))
	 ;; A&S 16.6.1
	 (sin u))
```

### `src/ellipt.lisp`, `DN`, compute the root without cancellation

**Replace** this block

```
	 ;; Note that (1-sqrt(1-mu))/(1+sqrt(1-mu)) is the same as
	 ;; -(mu+2*sqrt(1-mu)-2)/mu.  Also, the former is more
	 ;; accurate for small mu.
	 (let* ((root (let ((root-1-m (sqrt (- 1 m))))
			(/ (- 1 root-1-m)
			   (+ 1 root-1-m))))
```

with

```
	 ;; Note that (1-sqrt(1-mu))/(1+sqrt(1-mu)) is the same as
	 ;; -(mu+2*sqrt(1-mu)-2)/mu and as mu/(1+sqrt(1-mu))^2.  The
	 ;; last one is the most accurate for small mu.
	 (let* ((root (let ((root-1-m (sqrt (- 1 m))))
			(/ m (expt (+ 1 root-1-m) 2))))
```

## 5. Proposed test cases

All in `tests/rtest_elliptic.mac`. With the fix, the file passes 260/260
(259 plus the new test), with and without the bigfloat square root patch
in `handover-profiling-speedups.md`, except 75, 235 and 254, which that
handover changes. With HEAD's `jacobi` functions, the new versions of
240 to 244 and 246 to 250 and the new test fail. `make check` passes with
this and the `limit()` fix (21,671 tests, dependency check included).

If the square root patch is already in, 245 and 250 look different there.
Replace them with the versions below all the same. 245 is the same as
in that handover, and 250 can use the much smaller tolerance here.

`tests/rtest_elliptic.mac` 236: the literal is correct to 5e-37. The tolerance was just above 1 ulp. The value is 4 ulp off with the fix (1 ulp before), so allow about 13 ulp.

Old:

```
closeto(jacobi_am(0.5b0, 1.5b0) - 0.470719789704699163132257860869655024b0, 7.7038b-34);
true$
```

New:

```
closeto(jacobi_am(0.5b0, 1.5b0) - 0.470719789704699163132257860869655024b0, 1b-32);
true$
```

`tests/rtest_elliptic.mac` 237: the true error is 2.25e-32 with and without the fix, but the tolerance was 0.5% above the old result, and with the fix and the square root patch together the rounding goes over it.

Old:

```
closeto(jacobi_am(1.5b0, 1.5b0+%i) - (0.934054216870078303274350830308588452b0 - 0.372396045214607165539500260724230487b0*%i), 2.3112b-32);
true$
```

New:

```
closeto(jacobi_am(1.5b0, 1.5b0+%i) - (0.934054216870078303274350830308588452b0 - 0.372396045214607165539500260724230487b0*%i), 5b-32);
true$
```

`tests/rtest_elliptic.mac` 240 to 250: instead of the identity
`sin(jacobi_am(z, m)) = jacobi_sn(z, m)`, check both sides against correct
values of `sn(z, m)`. Tolerances are at least twice the largest error with
the fix, with and without the square root patch.

Problem 240, old:

```
makelist(block([z : 2*k, m : 2.5],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 2.2205e-16)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               0.23978245911126936,
               -0.4344064243713487,
               0.5624752048880169,
               -0.6243382655558817,
               0.6258523251112367,
               -0.5672144717423114,
               0.44267454374831006,
               -0.25118953999281624,
               0.01276742900276809,
               0.2282498061811287]],
  makelist(block([z : 2*k, m : 2.5],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 2e-14),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 2e-14)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 241, old:

```
makelist(block([z : 2*k, m : 1+.5*%i],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 6.7533e-16)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               1.042283877057475 - 0.1038915642562405*%i,
               -1.5546930299277562 + 0.742703174170198*%i,
               -0.7098200117678178 + 0.2041170455533357*%i,
               0.8744783521382082 + 0.0601186889344097*%i,
               1.1794450595186774 + 1.010951201421348*%i,
               -0.9280441610525201 + 0.215142892860914*%i,
               0.43575406793772903 + 0.7744119897866655*%i,
               0.9928728741143904 + 0.06058135569066523*%i,
               -0.7183903704476474 + 0.41432254522744083*%i,
               -0.9784250134788892 + 0.6134563283325234*%i]],
  makelist(block([z : 2*k, m : 1+.5*%i],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 1e-14),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 1e-14)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 242, old:

```
makelist(block([z : 2*k*%i, m : .75],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 3.1087e-14)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               -3.5680740402473776*%i,
               0.7123617855471847*%i,
               -0.8908946187600469*%i,
               2.5473361197579525*%i,
               -0.11494196865292264*%i,
               -5.713385195289199*%i,
               0.5575804632187636*%i,
               -1.1043101071442403*%i,
               1.9396148960362904*%i,
               -0.23257157765274064*%i]],
  makelist(block([z : 2*k*%i, m : .75],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 5e-14),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 5e-14)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 243, old:

```
makelist(block([z : 2*k*%i, m : 1.75],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 1.0659e-14)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               -0.9510999762907365*%i,
               9.766509047312184*%i,
               0.6622635396883445*%i,
               -0.15608478938811488*%i,
               -1.3824374272946258*%i,
               3.1839240941317266*%i,
               0.4438270268766484*%i,
               -0.3229495899544921*%i,
               -2.159753960376566*%i,
               1.82366723180081*%i]],
  makelist(block([z : 2*k*%i, m : 1.75],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 4e-13),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 4e-13)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 244, old:

```
makelist(block([z : 2*k*%i, m : 1.75+%i],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 1.4044e-15)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               -0.5721034235359987 - 0.6021109055201233*%i,
               -1.4362455608619944 + 0.11700759791999776*%i,
               -0.7624678543522865 + 0.30232418536392947*%i,
               -0.6913250228212221 + 0.1571290154239146*%i,
               -0.9657943331744151 + 0.1268004931592223*%i,
               -0.9833364721127776 - 0.17760813301545303*%i,
               -0.4585537156468036 - 0.07854576253310706*%i,
               -0.17631741088075936 + 0.49900657951616934*%i,
               1.597322749486544 + 2.489665295219922*%i,
               0.8280244930780284 - 0.8738216619745627*%i]],
  makelist(block([z : 2*k*%i, m : 1.75+%i],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 5e-14),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 5e-14)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 245, old:

```
makelist(block([z : 2*k, m : .5b0],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 1.8489b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               9.94662325358017683601254302053498830b-1,
               -2.85777915711237958898072295463821503b-1,
               -9.50974164149004587670557756887183760b-1,
               5.38215011375607382129700566218767082b-1,
               8.58812505952778731596037769874678293b-1,
               -7.36572976119784415625874479434183301b-1,
               -7.11120606110816077849800848330029327b-1,
               8.75695285305898245412937680262292166b-1,
               5.04142728502709650306522974280727257b-1,
               -9.60287786721909890934860303094132890b-1]],
  makelist(block([z : 2*k, m : .5b0],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 1.5b-31),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 1.5b-31)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 246, old:

```
makelist(block([z : 2*k, m : 2.5b0],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 1.5408b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               2.39782459111269345646596232602990922b-1,
               -4.34406424371348685521308215692883559b-1,
               5.62475204888016921206238346565343439b-1,
               -6.24338265555881664453199544606639330b-1,
               6.25852325111236683814918460071730481b-1,
               -5.67214471742311430906217603076230599b-1,
               4.42674543748310032306086512931299692b-1,
               -2.51189539992816212462386885865243904b-1,
               1.27674290027680896245980912871798411b-2,
               2.28249806181128696945882185544281869b-1]],
  makelist(block([z : 2*k, m : 2.5b0],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 2b-31),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 2b-31)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 247, old:

```
makelist(block([z : 2*k, m : 1+.5b0*%i],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 2.5271b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               1.04228387705747502966489676329538216b0 - 1.03891564256240508179031266498023910b-1*%i,
               -1.55469302992775608453910556130299731b0 + 7.42703174170198030107279538608192508b-1*%i,
               -7.09820011767817789190239731593974467b-1 + 2.04117045553335718021284756945794848b-1*%i,
               8.74478352138208285555527183577473357b-1 + 6.01186889344096959247192171344320793b-2*%i,
               1.17944505951867747774492950678824220b0 + 1.01095120142134805584846963592026890b0*%i,
               -9.28044161052520022999328783887188617b-1 + 2.15142892860914022448391204975401067b-1*%i,
               4.35754067937729017085569013142558571b-1 + 7.74411989786665531066255788972324956b-1*%i,
               9.92872874114390384086993996059278300b-1 + 6.05813556906652268736552741313918801b-2*%i,
               -7.18390370447647373881859304229296773b-1 + 4.14322545227440808744572733527419501b-1*%i,
               -9.78425013478889190675833752689364650b-1 + 6.13456328332523367775737163707298822b-1*%i]],
  makelist(block([z : 2*k, m : 1+.5b0*%i],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 3b-31),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 3b-31)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 248, old:

```
makelist(block([z : 2*k*%i, m : .75b0],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 5.5467b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               -3.56807404024737753276040726955854450b0*%i,
               7.12361785547184670161013700580849365b-1*%i,
               -8.90894618760046903895591456047005921b-1*%i,
               2.54733611975795244183614536218621391b0*%i,
               -1.14941968652922636830190578077770040b-1*%i,
               -5.71338519528919831423187052278778024b0*%i,
               5.57580463218763609125791278594014424b-1*%i,
               -1.10431010714424020637047650377484205b0*%i,
               1.93961489603629039092171669193683273b0*%i,
               -2.32571577652740638101695418968106239b-1*%i]],
  makelist(block([z : 2*k*%i, m : .75b0],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 4b-30),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 4b-30)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 249, old:

```
makelist(block([z : 2*k*%i, m : 1.75b0],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 2.2187b-31)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               -9.51099976290736532193610074744011440b-1*%i,
               9.76650904731218344964683857896323471b0*%i,
               6.62263539688344486646274112381552419b-1*%i,
               -1.56084789388114878079096013123651113b-1*%i,
               -1.38243742729462582920093334308716855b0*%i,
               3.18392409413172677550535128035121302b0*%i,
               4.43827026876648400732340305201253387b-1*%i,
               -3.22949589954492093468094287781964035b-1*%i,
               -2.15975396037656594807977539420358958b0*%i,
               1.82366723180081011179007613787136481b0*%i]],
  makelist(block([z : 2*k*%i, m : 1.75b0],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 3b-30),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 3b-30)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

Problem 250, old:

```
makelist(block([z : 2*k*%i, m : 1.75b0+%i],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 4.8135b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               -5.72103423535998727958019373033266031b-1 - 6.02110905520123281469893500645172105b-1*%i,
               -1.43624556086199438442502525331485902b0 + 1.17007597919997767395941561207354807b-1*%i,
               -7.62467854352286456198305764923936017b-1 + 3.02324185363929479211313523082241917b-1*%i,
               -6.91325022821222063674474488732064470b-1 + 1.57129015423914619697483681220484576b-1*%i,
               -9.65794333174415071357345450678257393b-1 + 1.26800493159222275355516292611889998b-1*%i,
               -9.83336472112777656992190024923096366b-1 - 1.77608133015453039714665379644542393b-1*%i,
               -4.58553715646803605307335052496046927b-1 - 7.85457625331070626791189048860725482b-2*%i,
               -1.76317410880759372631708702431579915b-1 + 4.99006579516169313026664590060526183b-1*%i,
               1.59732274948654412318481916287919482b0 + 2.48966529521992193162783920166078882b0*%i,
               8.28024493078028417849365960338059298b-1 - 8.73821661974562686608211314376421172b-1*%i]],
  makelist(block([z : 2*k*%i, m : 1.75b0+%i],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 3b-30),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 3b-30)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

**New**, appended at the end of the file:

```
/* Bug #NNNN: "jacobi_sn and jacobi_am lose many digits for larger arguments" */

block([z : 20.0*%i],
  [closeto(jacobi_dn(z, 0.75) - 1.020081934968419, 2e-14),
   closeto(jacobi_cn(z, 0.75) - 1.026688627935405, 2e-14),
   closeto(jacobi_dn(z, 1.75+%i) - (1.0765867842487509 + 1.2123267066947074*%i), 2e-14),
   closeto(jacobi_cn(z, 1.75+%i) - (-1.2004970668366945 - 0.6027051283045847*%i), 2e-14)]);
[true, true, true, true]$
```

## 6. Proposed Git commit message

```
Fix jacobi_sn() and friends losing digits

jacobi_sn(), jacobi_cn(), jacobi_dn() and jacobi_am() lost many digits
for arguments with a larger imaginary part, and for real arguments with
m > 1. For example, jacobi_sn(20.0*%i, 1.75+%i) was wrong from the 4th
digit on, and with bigfloats about 12 digits were lost. The Landen
transformation behind them lost accuracy through a cancellation and
stopped too early for such arguments. Both are fixed.

The tests in rtest_elliptic that only checked sin(jacobi_am(z, m)) =
jacobi_sn(z, m) could not see this. They now compare with correct
values.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
```
