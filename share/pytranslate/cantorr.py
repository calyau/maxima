from pytranslate import *

#########################
### Cantorr2 Function ###
#########################
def block17340(v):
    v = Stack({}, v)
    if ((v["x"] > 0) and (v["x"] <= (1 / 3))):
            v["ret"] = cantorr2((3 * v["x"]), (v["n"] + (-1)))
    if (((1 / 3) < v["x"]) and (v["x"] < (2 / 3))):
            v["ret"] = 1
    if ((v["x"] >= (2 / 3)) and (v["x"] < 1)):
            v["ret"] = (1 + cantorr2(((3 * v["x"]) + (-2)), (v["n"] + (-1))))
    return((v["ret"] / 2))
def cantorr2(x, n, v = v):
    v = Stack({}, v)
    v.ins({"x" : x, "n" : n})
    v.ins({"ret" : 0, "k" : 0})
    if not(f["numberp"](v["x"])):
            return(f["cantorr2"](v["x"], v["n"]))
    if (v["x"] == 0):
            return(0)
    if (v["x"] == 1):
            return(1)
    f["print"](v["x"], v["n"])
    return((block17340(v) if (v["n"] > 0) else v["x"]))
f["cantorr2"] = cantorr2


#########################
### Cantorri Function ###
#########################
def block34784(v):
    v = Stack({}, v)
    v["ret"] = 0
    return(v["ret"])
def cantorri(x, n, v = v):
    v = Stack({}, v)
    v.ins({"x" : x, "n" : n})
    v.ins({"ret" : 1, "q" : None})
    if not(f["numberp"](v["x"])):
            return(f["cantorri"](v["x"], v["n"]))
    if ((v["x"] == 0) or (v["x"] == 1)):
            return(1)
    v["x"] = f["mod"](v["x"], 1)
    for v["i"] in range(1, (v["n"] + 1)):
        v["x"] = (3 * v["x"])
        v["q"] = math.floor(v["x"])
        
        if (v["q"] == 1):
                    return(block34784(v))
        v["x"] = (v["x"] + (-v["q"]))
    return(v["ret"])
f["cantorri"] = cantorri

#########################
### Cantorrd Function ###
#########################
def block47665(v):
    v = Stack({}, v)
    if ((v["x"] > 0) and (v["x"] <= (1 / 3))):
            v["ret"] = cantorrd((3 * v["x"]), (v["n"] + (-1)))
    if (((1 / 3) < v["x"]) and (v["x"] < (2 / 3))):
            v["ret"] = 0
    if (((2 / 3) <= v["x"]) and (v["x"] < 1)):
            v["ret"] = cantorrd(((3 * v["x"]) + (-2)), (v["n"] + (-1)))
    return(v["ret"])
def cantorrd(x, *n, v = v):
    v = Stack({}, v)
    v.ins({"x" : x, "n" : list(n)})
    v.ins({"ret" : 0})
    if f["emptyp"](v["n"]):
            v["n"] = v["fpprec"]
    else:
            v["n"] = f["first"](v["n"])
    if not(f["numberp"](v["x"])):
            return(f["cantorrd"](v["x"], v["n"]))
    if (v["x"] == 0):
            return(1)
    if (v["x"] == 1):
            return(1)
    v["x"] = f["mod"](v["x"], 1)
    return((block47665(v) if (v["n"] > 0) else 1))
f["cantorrd"] = cantorrd

########################
## Cantorr_p Function ##
########################
def block71117(v):
    v = Stack({}, v)
    v["p"] = f["denom"](v["b"])
    v["q"] = f["num"](v["b"])
    if ((v["x"] > 0) and (v["x"] < v["b"])):
            v["ret"] = cantorr_p((v["p"] * v["x"]), v["b"], (v["n"] + (-1)))
    if ((v["b"] <= v["x"]) and (v["x"] <= (1 + (-v["b"])))):
            v["ret"] = 1
    if ((v["x"] > (1 + (-v["b"]))) and (v["x"] < 1)):
            v["ret"] = (1 + cantorr_p(((v["p"] * v["x"]) + (-(v["p"] + (-v["q"])))), v["b"], (v["n"] + (-1))))
    return((v["ret"] / 2))
def cantorr_p(x, b, n, v = v):
    v = Stack({}, v)
    v.ins({"x" : x, "b" : b, "n" : n})
    v.ins({"ret" : 0, "p" : None, "q" : None, "d" : None})
    if not(f["numberp"](v["x"])):
            return(f["cantorr_p"](v["x"], v["b"], v["n"]))
    if (v["x"] == 0):
            return(0)
    if (v["x"] == 1):
            return(1)
    if (v["b"] > (1 / 2)):
            v["b"] = (1 + (-f["mod"](v["b"], 1)))
    return((block71117(v) if (v["n"] > 0) else v["x"]))

#########################
## gcantorseq Function ##
#########################
def block85290(v):
    v = Stack({}, v)
    v["s"] = f["append"](gcantorseq(v["u"], (v["q"] + (-(v["p"] / 2))), (v["p"] * v["r"]), v["r"], (v["n"] + (-1))), gcantorseq((v["q"] + (v["p"] / 2)), v["w"], (v["p"] * v["r"]), v["r"], (v["n"] + (-1))))
    return(v["s"])
def gcantorseq(u, w, p, r, n, v = v):
    v = Stack({}, v)
    v.ins({"u" : u, "w" : w, "p" : p, "r" : r, "n" : n})
    v.ins({"s" : None, "q" : None})
    
    if not(f["integerp"](v["n"])):
            return(None)
    v["q"] = ((v["u"] + v["w"]) / 2)
    v["s"] = [v["u"], v["w"]]
    return((block85290(v) if (v["n"] > 1) else v["s"]))

########################
## cantorseq Function ##
########################
def cantorseq(n, v = v):
    v = Stack({}, v)
    v.ins({"n" : n})
    v.ins({"seq" : [0, 1], "l" : [], "r" : []})
    if not(f["integerp"](v["n"])):
            return(f["cantorseq"](v["n"]))
    for v["k"] in range(1, ((v["n"] + (-1)) + 1)):
        v["r"] = v["seq"]
        # Map divide using Lambda in the following
        v["l"] = (v["r"] / 3)
        v["r"] = f["reverse"]((1 + (-v["l"])))
        v["seq"] = f["append"](v["l"], v["r"])
    return(v["seq"])

f["plot2d"](lambda x, v = Stack({}, v): f["cantorr2"](x, 10), ['x', 0, 1])
f["plot2d"](lambda x, v = Stack({}, v): f["cantorrd"](x, 10), ['x', 0, 1])
f["plot2d"](lambda x, v = Stack({}, v): f["cantorri"](x, 10), ['x', 0, 1])
