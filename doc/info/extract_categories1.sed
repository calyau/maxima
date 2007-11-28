s/^@def\(fn\|vr\)  *{[^}]*}  *\([^[:blank:]]*\).*/items = ["\2"]/
s/^@def\(fn\|vr\)x  *{[^}]*}  *\([^[:blank:]]*\).*/if not "\2" in items: items.append ("\2")/
s/^@end def\(fn\|vr\)/items = []/
s/^@node  *\([^,]*\).*/items = ["\1"] # extracted from node/
s/@opencatbox//
s/@closecatbox//
s/@category{\([^}]*\)}\s*/\$foo = []\$for x in items: foo.append ([items[0], x])\$try: categories ["\1"] . extend (foo)\$except KeyError: categories ["\1"] = foo\$/g
