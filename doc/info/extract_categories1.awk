BEGIN { print "categories = {}" }

{for (i=1; i<=NF; i++) print $i;}

END {
    print "for key in categories.keys():"
    print " f = open (\"Category-\" + key + \".texi\", \"w\")"
    print " f.write (\"@anchor{Category: \" + key + \"}\\n\")"
    print " f.write (\"@opencatbox\\n\")"
    print " f.write (\"@b{Category: \" + key + \"}\\n\\n\")"
    print " items = categories [key]"
    print " if len (items) == 0: continue"
    print " f.write (\"@ref{Item: \" + items[0] + \"}\\n\")"
    print " for item in items [1:]:"
    print "  f.write (\"@html\\n&sdot;\\n@end html\\n\")"
    print "  f.write (\"@ref{Item: \" + item + \"}\\n\")"
    print " f.write (\"@closecatbox\")"
    print }

