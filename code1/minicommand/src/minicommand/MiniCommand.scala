package minicommand

// Mini-Command Language with Global Store
// Interpreter

// Grammar:
// PROGRAM ::= COMMANDLIST
// COMMANDLIST ::= COMMAND | COMMAND ; COMMANDLIST
// COMMAND ::= VARIABLE = EXPRESSION
//           | print VARIABLE
//           | while EXPRESSION : COMMANDLIST end
// EXPRESSION ::= NUMERAL | VARIABLE | ( EXPRESSION OPERATOR EXPRESSION )
// OPERATOR ::= + | -
// NUMERAL is a sequence of digits
// VARIABLE is a sequence of letters, but not ’print’, ’while’,  or ’end’

// Operator Tree (Abstract Syntax Tree):
// PTREE ::= CLIST
// CLIST ::= List[CTREE]
// CTREE ::= Assign(VAR,ETREE) | Print(VAR) | While(ETREE,CLIST)
// ETREE ::= NUMERAL | VAR | Add(ETREE,ETREE) | Sub(ETREE,ETREE)

trait OpTree {
  sealed abstract class Etree
  case class Number(s: String) extends Etree
  case class Var(x: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  
  sealed abstract class Ctree
  case class Assign(x: String, e: Etree) extends Ctree
  case class Print(x: String) extends Ctree
  case class While(e: Etree, c: List[Ctree]) extends Ctree
}

import scala.util.parsing.combinator.JavaTokenParsers

object MiniCommand extends JavaTokenParsers with OpTree {
  // Parser
  def parse(source: String): List[Ctree] =
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }
  def prog: Parser[List[Ctree]] = commlist
  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")
  def comm: Parser[Ctree] = ident~("="~>expr) ^^
                             { case x~e => Assign(x,e) } |
                           "print"~>ident ^^
                             { case x => Print(x) } |
                           ("while"~>expr<~":")~(commlist<~"end") ^^
                             { case e~cs => While(e,cs) }
  def expr: Parser[Etree] = wholeNumber ^^ (Number(_)) |
                           ident ^^ (Var(_)) |
                           "("~>expr~op~expr<~")" ^^
                           { case e1~"+"~e2 => Add(e1,e2)
                             case e1~"-"~e2 => Sub(e1,e2) }
  def op: Parser[String] = "+" | "-"

  // Interpreter
  def interpretPTREE(p: List[Ctree]): Map[String,Int] =
    interpretCLIST(p,Map())
  def interpretCLIST(cs: List[Ctree], ns: Map[String,Int]): Map[String,Int] = 
    cs match {
      case Nil => ns
      case c::cs => interpretCLIST(cs,interpretCTREE(c,ns))
    }
  def interpretCTREE(c: Ctree, ns: Map[String,Int]): Map[String,Int] =
    c match {
      case Assign(x,e) => ns + (x -> interpretETREE(e,ns))
      case Print(x) =>
        if (ns.contains(x)) println(ns(x))
        else throw new Exception("Error: " + x + " is undefined.")
        ns
      case While(e,cs) =>
        if (interpretETREE(e,ns) != 0)
          interpretCTREE(c,interpretCLIST(cs,ns))
        else ns
    }
  def interpretETREE(e: Etree, ns: Map[String,Int]): Int = e match {
     case Number(s) => s.toInt
     case Var(x) => if (ns.contains(x)) ns(x)
                   else throw new Exception("Error: " + x + " is undefined.")
     case Add(e1,e2) => interpretETREE(e1,ns) + interpretETREE(e2,ns)
     case Sub(e1,e2) => interpretETREE(e1,ns) - interpretETREE(e2,ns)
  }

  // Controller
  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      println("final namespace : " + interpretPTREE(optree))
    }
    catch { case e: Exception => println(e) }
  }
}