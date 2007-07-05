s/^@def\(fn\|vr\) {[^}]*} \([^[:blank:]]*\).*/item = "\2"/
s/^@end def\(fn\|vr\)/# item = ""/
s/@opencatbox//
s/@closecatbox//
s/@category{\([^}]*\)}\s*/\$try: categories ["\1"] . append (item)\$except KeyError: categories ["\1"] = [item]\$/g
