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
import java.lang.IllegalArgumentException;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

public class Templates {
    public static STGroup theGroup=null;
    public Templates(String g) throws Exception {
	theGroup = new STGroupFile(g);
    }
    public ST getInstanceOf(String n) throws IllegalArgumentException {
	ST t = theGroup.getInstanceOf(n);
	if(t == null){
	    System.err.println("Missing template "+n);
	    throw new IllegalArgumentException(n);
	}
	return t;
    }
}
