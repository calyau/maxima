# compute the standard deviation of a list of numbers
sigma := proc(data)
local mean,n,s,x;
n := nops(data);
if n < 2 then ERROR (`input must contain at least 2 values`) fi;
mean := 0;
for x in data do mean := mean + x od;
mean := mean/n;
s := 0;
for x in data do s := s + (x-mean)^2 od;
sqrt(s/(n-1))
end:
sigma([1,2,3,4,5,6]);
sigma([0.5,3.2,5.1]);
quit:
