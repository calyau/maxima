TARGET_TEXI=$1
if [ "x$TARGET_TEXI" = "x" ]; then
  echo USAGE: sh $0 '<TARGET_TEXI>'
  exit 1
fi

set -x
WORKING_DIRECTORY=`mktemp -d /tmp/maxima-texinfo-categories-XXXXXX`
cp -a *.texi figures $WORKING_DIRECTORY
d=`pwd`
pushd $WORKING_DIRECTORY

for f in *.texi; do
  sed 's/^@def\(fn\|vr\) {[^}]*} \([^[:blank:]]*\).*/@anchor{Item: \2}\
\0/' "$f" > tmp.texi
  mv tmp.texi "$f"
done

cat *.texi\
  | awk '!/^@def(fn|vr)x/ && (/^@deffn/||/^@defvr/||/^@end deffn/||/^@end defvr/ || /@category/)'\
  | sed -f $d/extract_categories1.sed \
  | awk -F '$' -f $d/extract_categories1.awk \
  > tmp-make-categories.py

python tmp-make-categories.py

sed 's/^@bye//' $TARGET_TEXI > tmp-target.texi
echo '@node Categories' >> tmp-target.texi
echo '@section Categories' >> tmp-target.texi
for f in Category-*.texi; do echo '@include' $f; done >> tmp-target.texi
echo '@bye' >> tmp-target.texi
mv tmp-target.texi $TARGET_TEXI

perl $d/texi2html -split_chapter --lang=en --output=. \
 --css-include=$d/manual.css --init-file $d/texi2html.init $TARGET_TEXI

popd
set +x
