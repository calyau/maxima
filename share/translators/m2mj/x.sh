antlr4 mapleLexer.g4
antlr4 -visitor mapleParser.g4
javac -cp /usr/local/lib/antlr-4.9.2-complete.jar:. *.java
java -cp /usr/local/lib/antlr-4.9.2-complete.jar:. M2M test.mpl
java -cp /usr/local/lib/antlr-4.9.2-complete.jar:. M2M pmint.mpl
