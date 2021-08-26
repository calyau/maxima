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
import java.util.List;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

public class mapleParserVisitorImpl extends mapleParserBaseVisitor<ST>{
 @Override
 public ST visitProgram(mapleParserParser.ProgramContext ctx){
  //say("program");
  ST t = visit(ctx.statseq());
  say(t.render());
  return null;
 }
 @Override
 public ST visitStatseq(mapleParserParser.StatseqContext ctx){
  //say("statseq");
  ST t = M2M.theTemplates.getInstanceOf("statseq");
  for(mapleParserParser.StatContext stat : ctx.stat()){
   //System.out.println(stat.getText());
   t.add("stat",visit(stat));
  }
  //t.inspect();
  return t;
 }
 @Override
 public ST visitProcStat(mapleParserParser.ProcStatContext ctx){ 
  //say("visitproc");
  ST s=null;
  ST t = M2M.theTemplates.getInstanceOf("proc");
  t.add("name",ctx.nameseq().name().get(0).getText());
  for(mapleParserParser.OneparmContext parm : ctx.parmseq().oneparm()){
   t.add("parms", parm.getText());
  }
  t.add("locals", visit(ctx.decls_proc()));
  t.add("statseq", s=visit(ctx.statseq()));
  //s.inspect();
  return t;
 }
 @Override
 public ST visitLocals_of_proc(mapleParserParser.Locals_of_procContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("local");
  for(mapleParserParser.NameContext name : ctx.nameseq().name()){
   t.add("names", name.getText());
  }
  return t;
 }
 @Override
 public ST visitIfStat(mapleParserParser.IfStatContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("ifstat");
  t.add("expr",visit(ctx.expr()));
  t.add("statseq",visit(ctx.statseq()));
  mapleParserParser.Elif_clauseContext elifClause = ctx.elif_clause();
  if(elifClause !=null)t.add("elifclause",visit(elifClause));
  mapleParserParser.Else_clauseContext elseClause = ctx.else_clause();
  if(elseClause !=null)t.add("elseclause",visit(elseClause));
  return t;
 }
 @Override
 public ST visitElse_clause(mapleParserParser.Else_clauseContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("elseclause");
  t.add("s",visit(ctx.statseq()));
  return t;
 }
 @Override
 public ST visitElif_clause(mapleParserParser.Elif_clauseContext ctx) {
  ST e = M2M.theTemplates.getInstanceOf("exprseq");
  say("size" + ctx.expr().size());
  for(int i=0; i < ctx.expr().size(); i++)
  {
   ST t = M2M.theTemplates.getInstanceOf("elifclause");
   t.add("x",visit1("ifelifstat","expr",ctx.expr(i)));
   t.add("i",visit(ctx.statseq(i)));
   say("i "+i+" "+t.render());
   e.add("e",t);
  }
  return e;
 }
 @Override
 public ST visitSeqWithPrefix(mapleParserParser.SeqWithPrefixContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("notimplemented");
  t.add("name","SeqWithPrefix");
  return t;
 }
 @Override
 public ST visitSeqSansPrefix(mapleParserParser.SeqSansPrefixContext ctx) {
  //say("SeqWithoutPrefix");
  ST t = M2M.theTemplates.getInstanceOf("seqsansprefix");
  List<mapleParserParser.ExprContext> l = ctx.exprseq().expr();
  say("SeqWithoutPrefix"+l.size());
  if(l.size()<1){say("l empty"); return t; }
  mapleParserParser.ExprContext left = l.get(0);
  if(left == null){say("left null"); return t; }
  t.add("left",visit1("seqsansprefix","left",left));
  if(l.size()<2){say("r empty"); return t; }
  mapleParserParser.ExprContext right = l.get(1);
  if(right == null){say("right null"); return t; }
  t.add("right",visit1("seqsansprefix","right",right));
  return t;
 }
 /*
 @Override
 public ST visitIfOne(mapleParserParser.IfOneContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("ifone");
  t.add("expr",visit(ctx.expr()));
  t.add("statseq",visit(ctx.statseq()));
  return t;
 } */
 @Override
 public ST visitAssignStat(mapleParserParser.AssignStatContext ctx) {
  //say("visitAssignStat");
  ST t = M2M.theTemplates.getInstanceOf("assign");
  t.add("lvalue",ctx.nameseq().name().get(0).getText());
  t.add("exprseq",visit(ctx.exprseq()));
  //say(t.render());
  return t;
 }
 @Override
 public ST visitExprseq(mapleParserParser.ExprseqContext ctx) {
  say("visitExprseq");
  ST t = M2M.theTemplates.getInstanceOf("exprseq");
  for(mapleParserParser.ExprContext expr : ctx.expr()){
   say("expr "+expr.getText());
   t.add("e",visit1("expr","",expr));
  }
  return t;
 }
 @Override
 public ST visitNameBracket(mapleParserParser.NameBracketContext ctx){
  say("visitNameBracketContext");
  ST t = M2M.theTemplates.getInstanceOf("namebracket");
  t.add("n",visit(ctx.name()));
  for(mapleParserParser.ExprContext expr : ctx.exprseq().expr()){
   say("expr "+expr.getText());
   t.add("e",visit1("expr","",expr));
  }
  return t;
 }
 @Override
 public ST visitFunctionalOperatorExpr(mapleParserParser.FunctionalOperatorExprContext ctx) {
  //say("visitFunctionalOperatorExpr");
  ST t = M2M.theTemplates.getInstanceOf("function");
  t.add("name",ctx.name().getText());
  say(t.render());
  mapleParserParser.Functional_operatorContext f = ctx.functional_operator();
  if(f == null){say("f null"); return t; }
  List<mapleParserParser.ExprseqContext> l = f.exprseq();
  if(l.isEmpty()){say("f empty"); return t; }
  mapleParserParser.ExprseqContext e = l.get(0);
  if(e == null){say("e null"); return t; }
  List<mapleParserParser.ExprContext> l1 = e.expr();
  for(mapleParserParser.ExprContext expr : l1){
   t.add("args",visit1("visitFunctionalOperatorExpr", "<none>",expr));
  }
  return t;
 }
 @Override
 public ST visitSetExpr(mapleParserParser.SetExprContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("setexpr");
  for(mapleParserParser.ExprContext expr : ctx.exprseq().expr()){
   t.add("args",visit(expr));
  }
  return t;
 }
 @Override
 public ST visitListExpr(mapleParserParser.ListExprContext ctx) {
  say("listexpr");
  ST t = M2M.theTemplates.getInstanceOf("listexpr");
  for(mapleParserParser.ExprContext expr : ctx.exprseq().expr()){
   say("listexpr "+expr.getText());
   t.add("args",visit1("listexpr","",expr));
  }
  return t;
 }
 @Override
 public ST visitId(mapleParserParser.IdContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("name");
  t.add("text",ctx.getText());
  return t;
 }
 @Override
 public ST visitDoubleQuote(mapleParserParser.DoubleQuoteContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("name");
  t.add("text",ctx.getText());
  return t;
 }
 @Override
 public ST visitForBodyWhile(mapleParserParser.ForBodyWhileContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("forbodywhile");
  t.add("expr",visit(ctx.expr()));
  t.add("statseq",visit(ctx.statseq()));
  return t;
 }
 @Override
 public ST visitFor_in_stmt(mapleParserParser.For_in_stmtContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("forin");
  t.add("name",visit(ctx.name()));
  t.add("expr",visit(ctx.expr()));
  t.add("body",visit(ctx.for_body()));
  return t;

 }
 @Override
 public ST visitForBodyDo(mapleParserParser.ForBodyDoContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("forbodydo");
  t.add("statseq",visit(ctx.statseq()));
  return t;
 }
 @Override
 public ST visitBackQuote(mapleParserParser.BackQuoteContext ctx) {
  // Not sure how to tell the difference between back quote used as an
  // indentifier and back quote used to delimit a string.
  ST t = M2M.theTemplates.getInstanceOf("name");
  t.add("text",ctx.getText());
  return t;
 }
 @Override
 public ST visitQuote(mapleParserParser.QuoteContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("name");
  t.add("text",ctx.getText());
  return t;
 }
 @Override
 public ST visitFloatExpr(mapleParserParser.FloatExprContext ctx) {
  // Try to convert the string to a number and fix the format.
  // Use the input on failure.
  ST t = M2M.theTemplates.getInstanceOf("number");
  java.math.BigDecimal d = null;
  String s = ctx.getText();
  try{
    d = new java.math.BigDecimal(s);
    s = d.toString();
  }catch(NumberFormatException e){};
  t.add("text",s);
  return t;
 }
 public ST visit1(String visitor, String op, mapleParserParser.ExprContext c)
 {
  ST v = null;
  try{
   v = visit(c);
  } catch(Exception e){
   say(visitor+" "+op+" "+e.toString());
  };
  if(v==null){
   ST n = M2M.theTemplates.getInstanceOf("isnull");
   n.add("name",visitor);
   n.add("op",op);
   return n;
  }
  return v;
 }
 public ST binexp(String exptype, String op, mapleParserParser.ExprContext l, mapleParserParser.ExprContext r)
 {
  ST t = M2M.theTemplates.getInstanceOf(exptype);
  t.add("op",op);
  t.add("l",visit1(exptype, op, l));
  t.add("r",visit1(exptype, op, r));
  return t;
 }
 @Override
 public ST visitExpoOp(mapleParserParser.ExpoOpContext ctx) {
  return binexp("binexp", ctx.OP.getText(),ctx.expr(0),ctx.expr(1));
 }
 @Override
 public ST visitMultOp(mapleParserParser.MultOpContext ctx) {
  return binexp("binexp", ctx.OP.getText(),ctx.expr(0),ctx.expr(1));
 }
 @Override
 public ST visitAddOp(mapleParserParser.AddOpContext ctx) {
  return binexp("binexp", ctx.OP.getText(),ctx.expr(0),ctx.expr(1));
 }
 @Override
 public ST visitBinaryRelOp(mapleParserParser.BinaryRelOpContext ctx) {
  // Not sure if this is the best place to change this.
  String op = ctx.OP.getText().equals("<>")?"#":ctx.OP.getText();
  return binexp("binexp", op,ctx.expr(0),ctx.expr(1));
 }
 public ST visitSetRelOp(mapleParserParser.SetRelOpContext ctx) {
  return binexp("setrelexp", ctx.OP.getText(),ctx.expr(0),ctx.expr(1));
 }
 @Override
 public ST visitProcExpr(mapleParserParser.ProcExprContext ctx){
  ST t = M2M.theTemplates.getInstanceOf("procexpr");
  for(mapleParserParser.OneparmContext parm : ctx.parmseq().oneparm()){
   t.add("parms", parm.getText());
  }
  t.add("expr",visit1("procexpr","<none>",ctx.expr()));
  //say(t.render());
  return t;
 }
 @Override
 public ST visitParenExpr(mapleParserParser.ParenExprContext ctx) {
  ST t = M2M.theTemplates.getInstanceOf("paren");
  t.add("e",visit(ctx.exprseq().expr(0)));
  return t;
 }
 @Override
 public ST visitIntExpr(mapleParserParser.IntExprContext ctx) {
  // Try to convert the string to a number and fix the format.
  // Use the input on failure.
  ST t = M2M.theTemplates.getInstanceOf("number");
  java.math.BigInteger d = null;
  String s = ctx.getText();
  try{
    d = new java.math.BigInteger(s);
    s = d.toString();
  }catch(NumberFormatException e){};
  t.add("text",s);
  return t;
 }
 public void say(String s){ System.out.println(s); }
}
