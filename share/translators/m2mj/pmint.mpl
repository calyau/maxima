# The Poor Man's Integrator, a parallel integration heuristic
# Version 1.1 ---  May 10, 2005  (c) M.Bronstein and INRIA 2004-2005
pmint := proc(f,x)
	local ff, si, li, lin, lout, ld, q, d, l, vars, dx, ls, fint, lc;
	ff := eval(convert(f, tan));		# convert trigs to tan
	si := select(proc(d) diff(d,x) <> 0 end, indets(ff));
	si := select(proc(d) diff(d,x) <> 0 end, indets(map(diff, si, x))) union si;
	li := [op(si)];		# list of terms in integrand and its derivative
	lin := [seq(d=`tools/genglobal`(x), d=li)];	# substitution terms->indets
	lout := [seq(rhs(d)=lhs(d), d=lin)];		# substitution indets->terms
	ld := subs(lin, map(diff, li, x));	# derivatives of all the terms
	q := lcm(seq(denom(d), d=ld));		# denominator of the total derivation
	l := [seq(normal(q * d), d=ld)];	# q * derivatives of all the terms
	vars := map(lhs, lout);
	dx := totalDerivation(vars, l);		# vector field dx = q * d/dx
	ls := [seq(getSpecial(d, lin), d=li)];	# list of known Darboux for dx
	fint := subs(lout, pmIntegrate(subs(lin, ff), dx, q, vars, ls));
	lc := select(proc(d) convert(d,string)[1]="_"; end, indets(fint, name));
	subs({seq(d = 0, d=lc minus si)}, fint);
end;

getSpecial := proc(f, l) local p;		# return known Darboux polys
	p := op(0,f);
	if p = `tan` then [1+subs(l,f)^2, false];
	elif p = `tanh` then [1 + subs(l,f), false], [1 - subs(l,f), false];
	elif p = `LambertW` then [subs(l,f), true];
	else NULL; fi;
end;

totalDerivation := proc(lv, ld)
	proc(f) local fp, i;
		fp := 0; for i to nops(ld) do fp := fp + ld[i] * diff(f, lv[i]); od;
		fp;
	end;
end;

pmIntegrate := proc(f, d, q, vars)
	local ls, splq, s, ff, df, spl, cden, dg, monomials, cand, lunk, sol, i;
	if nargs = 5 then ls := args[5]; else ls := []; fi;
	splq := splitFactor(q, d);
	s := splq[1]; for i to nops(ls) do if ls[i][2] then s := s*ls[i][1]; fi; od;
	ff := normal(f); df := denom(ff); spl := splitFactor(df, d);
	cden := s * spl[1] * deflation(spl[2], d);
	dg := 1 + degree(s) +  max(degree(numer(ff)), degree(denom(ff)));
	monomials := [op(enumerateMonoms(vars, dg))];
	cand := add('_A'[i] * monomials[i], i = 1..nops(monomials)) / cden;
	lunk := { seq('_A'[i], i = 1..nops(monomials)) };
	sol:= tryIntegral(f, d, q, vars, cand, lunk, spl[1], spl[2], splq[1], ls, 0);
	if sol[1] then sol := tryIntegral(f, d, q, vars, cand, lunk,
						spl[1], spl[2], splq[1], ls, I); fi;
	if sol[1] then Int(f); else sol[2]; fi;
end;

tryIntegral := proc(f, d, q, vars, cand, lunk, l1, l2, l3, ls, K)
	local candlog, p, candidate, i, sol;
	candlog := [op({ myfactors(l1, K), myfactors(l2, K), myfactors(l3, K) }
			union { seq(p[1], p=ls) })];
	candidate := cand + add('_B'[i] * log(candlog[i]), i = 1..nops(candlog));
	sol := solve({coeffs(numer(normal(f - d(candidate)/q)), {op(vars)})},
				lunk union { seq('_B'[i], i = 1..nops(candlog)) });
	[evalb(sol=NULL), subs(sol,candidate)];
end;

myfactors := proc(p, K) local l, fact;
	if K = 0 then l := factors(p); else l := factors(p, K); fi;
	seq(fact[1], fact=l[2]);
end;

enumerateMonoms := proc(vars, d) local n, x, i, v, s, w;
	n := nops(vars);
	if n = 0 then {1}; else
		x := vars[n];
		v := [seq(vars[i], i = 1..n-1)];
		s := enumerateMonoms(v, d);
		for i to d do s := s union {seq(x^i*w,w=enumerateMonoms(v,d-i))}; od;
		s;
	fi;
end;

splitFactor := proc(p, d) local si, x, c, q, spl, s, splh;
	si := select(proc(z) d(z) <> 0 end, indets(p,name));
	if si = {} then RETURN([1,p]) fi;
	x := si[1];
	c := content(p, x, 'q');
	spl := splitFactor(c, d);
	s := normal(gcd(q, d(q)) / gcd(q, diff(q, x)));
	if degree(s) = 0 then RETURN([spl[1], q * spl[2]]); fi;
	splh := splitFactor(normal(q / s), d);
	[spl[1] * splh[1] * s, spl[2] * splh[2]];
end;

deflation := proc(p, d) local si, x, c, q;
	si := select(proc(z) d(z) <> 0 end, indets(p,name));
	if si = {} then RETURN(p) fi;
	x := si[1];
	c := content(p, x, 'q');
	deflation(c, d) * gcd(q, diff(q, x));
end;
