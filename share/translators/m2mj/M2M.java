/* François Thomasset -- INRIA Rocquencourt -- Octobre 2001 *)

(* Translation from Maple to MuPad : syntaxic specification of maple *)

(*
Copyright © 2001-2002 François Thomasset, all rights reserved.
All of Dan Stanger's changes are Copyright © 2021 Dan Stanger, all rights reserved.
Copying is covered by the GNU General Public License (GPL).
 
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details. */
/*
 At this time, the file is preprocessed to remove any backspaces.
 The translation does not handle end of line comments yet.
*/
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

import java.io.FileInputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class M2M {
    public static Templates theTemplates=null;
    public static TokenStreamRewriter rewriter;
    public static void main(String[] args) throws Exception {
	theTemplates = new Templates("Macsyma.stg");
	FileInputStream fis = new FileInputStream(args[0]);
	PrintWriter pw = new PrintWriter(args[1]);
	StringBuilder sb = preprocess(fis);
	ANTLRInputStream input = new ANTLRInputStream(sb.toString());
	mapleLexer lexer = new mapleLexer(input);
	CommonTokenStream tokens = new CommonTokenStream(lexer);
	mapleParserParser parser = new mapleParserParser(tokens);
	rewriter = new TokenStreamRewriter(tokens);
	parser.setTrace(true);
	ParseTree tree = parser.program();
	ParseTreeWalker walker = new ParseTreeWalker();
	mapleParserVisitorImpl visitor = new mapleParserVisitorImpl();
	ST t=visitor.visit(tree);
	pw.println(t.render());
	pw.flush();
	pw.close();
	//System.out.println(tree.toStringTree(parser));
    }
    public static StringBuilder preprocess(FileInputStream fis) throws Exception
    {
	StringBuilder b = new StringBuilder();
	try(BufferedReader br = new BufferedReader(new InputStreamReader(fis))) {
	    for(String line; (line = br.readLine()) != null; ) {
		line.replace('\b',' ');
		int i=-1;
		if(line.indexOf('#')==0)line=line+'\b';
		else if ((i=line.indexOf("#")) > 0){
			line=line.substring(0,i-1); // Delete the comment
		}
		b.append(line+'\n');
	    }
	}
	return b;
    }
}
