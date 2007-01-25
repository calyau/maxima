#!/bin/bash
#Your local directory is recorded in the generated files
#maxima.info -1 and in somes tmp-html files. Remove it using sed 
#before send to internet
TEXINFO="maxima.texi"
texi2html --lang=pt --split=chapter --output=tmp-html $TEXINFO
makeinfo --enable-encoding $TEXINFO
LIST="maxima.html maxima_1.html maxima_2.html maxima_3.html maxima_4.html \
maxima_5.html maxima_6.html maxima_7.html maxima_8.html maxima_9.html \
maxima_10.html maxima_11.html maxima_12.html maxima_13.html maxima_14.html \
maxima_15.html maxima_16.html maxima_17.html maxima_18.html maxima_19.html \
maxima_20.html maxima_21.html maxima_22.html maxima_23.html maxima_24.html \
maxima_25.html maxima_26.html maxima_27.html maxima_28.html maxima_29.html \
maxima_30.html maxima_31.html maxima_32.html maxima_33.html maxima_34.html \
maxima_35.html maxima_36.html maxima_37.html maxima_38.html maxima_39.html \
maxima_40.html maxima_41.html maxima_42.html maxima_43.html maxima_44.html \
maxima_45.html maxima_46.html maxima_47.html maxima_48.html maxima_49.html \
maxima_50.html maxima_51.html maxima_52.html maxima_53.html maxima_54.html \
maxima_55.html maxima_56.html maxima_57.html maxima_58.html maxima_59.html \
maxima_60.html maxima_61.html maxima_62.html maxima_63.html maxima_64.html \
maxima_65.html maxima_66.html maxima_67.html maxima_68.html maxima_69.html \
maxima_70.html maxima_71.html maxima_72.html maxima_73.html maxima_74.html \
maxima_75.html maxima_76.html maxima_77.html maxima_78.html maxima_79.html \
maxima_80.html maxima_81.html maxima_82.html maxima_83.html maxima_84.html \
maxima_85.html maxima_86.html maxima_87.html maxima_abt.html maxima_fot.html \
maxima_ovr.html maxima_toc.html"
bin/htmldoc --numbered --quiet --path $HTML_DIR/../figures -t pdf --book --webpage -f maxima.pdf $LIST
unset TEXINFO LIST
