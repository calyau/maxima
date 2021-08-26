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
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

import java.io.FileInputStream;
import java.io.InputStream;

public class M2M {
    public static Templates theTemplates=null;
    public static void main(String[] args) throws Exception {
	theTemplates = new Templates("Macsyma.stg");
	FileInputStream fis = new FileInputStream(args[0]);
	ANTLRInputStream input = new ANTLRInputStream(fis);
	mapleLexer lexer = new mapleLexer(input);
	CommonTokenStream tokens = new CommonTokenStream(lexer);
	mapleParserParser parser = new mapleParserParser(tokens);
	parser.setTrace(true);
	ParseTree tree = parser.program();
	ParseTreeWalker walker = new ParseTreeWalker();
	mapleParserVisitorImpl visitor = new mapleParserVisitorImpl();
	visitor.visit(tree);
	//System.out.println(tree.toStringTree(parser));
    }
}
