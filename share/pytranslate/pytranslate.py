from collections.abc import MutableMapping
import math

class HierarchialDict(MutableMapping):
    def __init__(self, data={}, sub={}):
        self.mapping = data
        self.sub = sub
    def __getitem__(self, key):
        if(key in self.mapping):
            return self.mapping[key]
        else:
            return self.sub[key]
    def __delitem__(self, key):
        if key in self.mapping:
            del self.mapping[key]
        else:
            del self.sub[key]
    def __setitem__(self, key, value):
        if key in self.mapping:
            self.mapping[key] = value
        else:
            self.sub[key] = value
        return(value)
    def __iter__(self):
        return iter({**self.sub, **self.mapping})
    def __len__(self):
        return len(self.mapping)+len(self.sub)
    def __repr__(self):
        return f"{self.mapping}, sub:{self.sub}"
    def ins(self, data):
        self.mapping={**self.mapping, **data}

m_vars = {
    'fpprec': 16,
    'pi': math.pi,
    'e': math.e,
    'ratepsilon': 2.0E-15
}

m_funcs = {
    'sin': math.sin,
    'pow': math.pow,
    'factorial': math.factorial,
    'floor': math.floor,
    'sqrt': math.sqrt,
    'num':lambda x:x,
    'denom': lambda x:1,
    'print': print
}

def assign(lhs, rhs, var_map = m_vars):
    var_map[lhs] = rhs
    return(rhs)


