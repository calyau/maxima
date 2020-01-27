s/^@deffn  *{[^}]*}  *\([^[:blank:]]*\).*/items = [["deffn", "\1"]]/
s/^@defvr  *{[^}]*}  *\([^[:blank:]]*\).*/items = [["defvr", "\1"]]/
s/^@deffnx  *{[^}]*}  *\([^[:blank:]]*\).*/if not ["deffn", "\1"] in items: items.append (["deffn", "\1"])/
s/^@defvrx  *{[^}]*}  *\([^[:blank:]]*\).*/if not ["defvr", "\1"] in items: items.append (["defvr", "\1"])/
s/^@end deffn/items = []/
s/^@end defvr/items = []/
s/^@node  *\([^,]*\).*/items = [["node", "\1"]] # extracted from node/
s/@opencatbox//
s/@closecatbox//
s/@category{\([^}]*\)}\s*/@@@###@@@foo = []@@@###@@@for x in items: foo.append ([filenamebase, x[0], x[1]])@@@###@@@try: categories ["\1"] . extend (foo)@@@###@@@except KeyError: categories ["\1"] = foo@@@###@@@/g
