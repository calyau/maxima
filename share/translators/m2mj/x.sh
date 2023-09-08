./antlr4 mapleLexer.g4
./antlr4 -visitor mapleParser.g4
javac -encoding ISO8859_1 -cp /usr/local/lib/antlr-4.13.0-complete.jar:. *.java
jar -c  -f m2m.jar m*.class
rm m*.class
java -cp /usr/local/lib/antlr-4.13.0-complete.jar:m2m.jar:. M2M test.mpl test.mac >test.trace
java -cp /usr/local/lib/antlr-4.13.0-complete.jar:m2m.jar:. M2M pmint.mpl pmint.mac >pmint.trace
