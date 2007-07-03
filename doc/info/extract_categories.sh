TARGET_TEXI=$1
if [ "x$TARGET_TEXI" = "x" ]; then
  echo USAGE: sh $0 '<TARGET_TEXI>'
  exit 1
fi

set -x
WORKING_DIRECTORY=`mktemp -d /tmp/maxima-texinfo-categories-XXXXXX`
cp -a *.texi manual.css texi2html.init figures $WORKING_DIRECTORY
pushd $WORKING_DIRECTORY

for f in *.texi; do
  sed 's/^@def\(fn\|vr\) {[^}]*} \(\w*\).*/@anchor{\2}\
\0/' "$f" > tmp.texi
  mv tmp.texi "$f"
done

cat *.texi\
  | awk '!/^@def(fn|vr)x/ && (/^@deffn/||/^@defvr/||/^@end deffn/||/^@end defvr/ || /@category/)'\
  | sed 's/^@def\(fn\|vr\) {[^}]*} \(\w*\).*/item = "\2"/; s/^@end def\(fn\|vr\)/# item = ""/; s/@category{\([^}]*\)}\s*/\$try: categories ["\1"] . append (item)\$except KeyError: categories ["\1"] = [item]\$/g'\
  | awk -F '$' 'BEGIN { print "categories = {}" }; {for (i=1; i<=NF; i++) print $i;} END { print "for key in categories.keys():"; print " f = open (\"category-\" + key + \".texi\", \"w\")"; print " f.write (\"@anchor{Category: \" + key + \"}\\n\")"; print " f.write (\"@b{Category: \" + key + \"}\\n\\n\")"; print " for item in categories [key]:"; print "  f.write (\"@ref{\" + item + \"}\\n\")"; print; }'\
  > tmp-make-categories.py

python tmp-make-categories.py

sed 's/^@bye//' $TARGET_TEXI > tmp-target.texi
echo '@node Categories' >> tmp-target.texi
echo '@section Categories' >> tmp-target.texi
for f in category-*.texi; do echo '@include' $f; done >> tmp-target.texi
echo '@bye' >> tmp-target.texi

perl /home/robert/tmp/maxima-head/maxima/doc/info/texi2html -split_chapter --lang=en --output=. --css-include=./manual.css --init-file ./texi2html.init tmp-target.texi

popd
set +x
