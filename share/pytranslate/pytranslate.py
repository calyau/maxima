import math
maxima_vars = {
    'fpprec': 16,
    'pi': math.pi,
    'e': math.e,
}

maxima_functions = {
    'sin': math.sin,
    'pow': math.pow,
    'factorial': math.factorial,
    'floor': math.floor,
    'sqrt': math.sqrt
}

def assign(var_map, lhs, rhs):
    var_map[lhs] = rhs
    return(var_map[lhs])


