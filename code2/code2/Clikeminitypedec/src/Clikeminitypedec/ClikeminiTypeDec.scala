package Clikeminitypedec

// Interpreter for a C-like mini-language with pointers
// augmented with simple int declarations
//
// Program P ::= CL
// CommandList CL ::= C | C ; CL
// Command C ::= L = E | while E : CL end | print L | T I
// Type T ::= int | T *
// Expression E ::= N | ( E1 + E2 ) | L | &L
// LefthandSide L ::= I | *L
// Numeral N ::= string of digits
// Variable I ::= strings of letters, not including keywords: while, print, end
//
// Operator Tree
// PTREE ::= CLIST
// CLIST ::= List[CTREE]
// CTREE ::= Assign(LTREE,ETREE) | While(ETREE,CLIST) | Print(LTREE) | Declare(VAR,TYPE)
// TYPE ::= List("int") | List("ptr","int") | List("ptr","ptr","int") | ...
// ETREE ::= Num(NUM) | Add(ETREE,ETREE) | Sub(ETREE,ETREE) | At(LTREE) | Amph(LTREE)
// LTREE ::= Var(VAR) | Star(LTREE)

trait OpTree {
  sealed abstract class Ctree
  case class Assign(l: LTree, e: Etree) extends Ctree
  case class While(e: Etree, cs: List[Ctree]) extends Ctree
  case class Print(l: LTree) extends Ctree
  case class Decl(x: String, t: List[String]) extends Ctree
  
  sealed abstract class Etree
  case class Num(n: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class At(l: LTree) extends Etree
  case class Amph(l: LTree) extends Etree
  
  sealed abstract class LTree
  case class Var(x: String) extends LTree
  case class Star(l: LTree) extends LTree
}

import scala.util.parsing.combinator.JavaTokenParsers

object ClikeminiTypeDec extends JavaTokenParsers with OpTree {
  // Parser
  def parse(source: String): List[Ctree] = 
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }
  def prog: Parser[List[Ctree]] = commlist
  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")
  def comm: Parser[Ctree] =    
    left~("="~>expr) ^^ { case l~e => Assign(l,e) } |
    "print"~>left ^^ { case l => Print(l) } |
    ("while"~>expr<~":")~(commlist<~"end") ^^ 
      { case e~cs => While(e,cs) } |
    typ~ident ^^ 
      { case t~x => Decl(x,t) } 
  def typ: Parser[List[String]] =
    "int" ^^ (List(_)) |
    typ<~"*" ^^ (List("ptr") ++ _)
  def expr: Parser[Etree] = 
    wholeNumber ^^ (Num(_)) | 
    "("~>expr~op~expr<~")" ^^
      { case e1~"+"~e2 => Add(e1,e2)
        case e1~"-"~e2 => Sub(e1,e2) } |
    left ^^ (At(_)) |
    "&"~>left ^^ (Amph(_))
  def left: Parser[LTree] =    
    ident ^^ (Var(_)) |
    "*"~>left ^^ (Star(_))
  def op: Parser[String] = "+" | "-"

  // Interpreter
  val memory = scala.collection.mutable.ArrayBuffer.empty[Int]
  var env = Map.empty[String,(List[String],Int)]
  def interpretPTREE(p: List[Ctree]): Unit = interpretCLIST(p)
  def interpretCLIST(cs: List[Ctree]): Unit = 
    for (c <- cs) yield interpretCTREE(c)
  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l,e) => {
      val (type1, lval) = interpretLTREE(l)
      val (type2, exprval) = interpretETREE(e)
      if (type1 == type2) 
        memory(lval) = exprval //  do the assignment
      else throw new Exception("incompatible types for assignment")
    }
    case Print(l) => {
      val (_,loc) = interpretLTREE(l)
      println(memory(loc))
    }
    case While(e,cs) => {
      val (_, cond) = interpretETREE(e)
      if (cond != 0) {
        interpretCLIST(cs)
        interpretCTREE(c)
      }
    }
    case Decl(x,typ) => {
      if (env contains x) throw new Exception("variable " ++ x ++ " redeclared")
      else 
        memory += 0 // add a cell at the end of memory
        env += (x -> (typ,memory.length - 1)) // save type and location for x
    }
  }
  def interpretETREE(e: Etree): (List[String], Int) = e match {
    case Num(n) => (List("int"), n.toInt)
    case Add(e1,e2) => 
      val (type1,n1) = interpretETREE(e1)
      val (type2,n2) = interpretETREE(e2)
      if (type1 == List("int") && type2 == List("int")) (List("int"), n1 + n2)
      else throw new Exception("cannot do arithmetic on non-ints")
    case Sub(e1,e2) =>
      val (type1,n1) = interpretETREE(e1)
      val (type2,n2) = interpretETREE(e2)
      if (type1 == List("int") && type2 == List("int")) (List("int"), n1 - n2)
      else throw new Exception("cannot do arithmetic on non-ints")
    case At(l) => 
      val (type0,n) = interpretLTREE(l)
      (type0, memory(n))
    case Amph(l) => 
      val (type0,n) = interpretLTREE(l)
      (List("ptr") ++ type0, n)
  }
  def interpretLTREE(l: LTree): (List[String], Int) = l match {
    case Var(x) => { 
      if (env contains x) env(x)
      else throw new Exception(x + " is not declared!")
    }
    case Star(l) => { // a pointer dereference
      val (datatype, loc) = interpretLTREE(l) // get a type and location number
      datatype match {
        case (h::t) => if (h == "ptr") (t,memory(loc))
                       else throw new Exception("variable not a pointer")
        case Nil => throw new Exception("no such a case occurs")
      } // dereference it and return the location therein
    }
  }
  // Controller
  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      interpretPTREE(optree)
      println("final memory : " + memory)
      println("final namespace : " + env)
    }
    catch { case e: Exception => println(e) }
  }
}
                           