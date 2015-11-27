// Interpreter for a C-like mini-language with pointers
// augmented with simple int and array declarations 
// (Homework #1)
//
//      Program  P ::= CL
//  CommandList CL ::= C | C ; CL
//  Declaration  D ::= int I | int [ N ] I
//      Command  C ::= D | L = E | while E : CL end | print L
//   Expression  E ::= N | ( E1 + E2 ) | L | &L
// LefthandSide  L ::= I | I [ E ] | *L
//      Numeral  N ::= string of digits
//     Variable  I ::= strings of letters, not including keywords: while, print, end
//
// Operator Tree
// PTREE ::= List[CTREE+]
// DTREE ::= VarDec(String) | ArrDec(String, Int)
// CTREE ::= Dec(DTREE) | Assign(LTREE,ETREE) | While(ETREE,CLIST) | Print(LTREE)
// ETREE ::= Num(String) | Add(ETREE,ETREE) | At(LTREE) | Amph(LTREE)
// LTREE ::= Var(String) | Arr(String,ETREE) | Star(LTREE)

trait OpTree {
  sealed abstract class Ctree
  case class Dec(d: Dtree) extends Ctree
  case class Assign(l: LTree, e: Etree) extends Ctree
  case class While(e: Etree, c: List[Ctree]) extends Ctree
  case class Print(L: LTree) extends Ctree
  
  sealed abstract class Dtree
  case class VarDec(x: String) extends Dtree
  case class ArrDec(x: String, n: String) extends Dtree
  
  sealed abstract class Etree
  case class Num(s: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class At(l: LTree) extends Etree
  case class Amph(l: LTree) extends Etree
  
  sealed abstract class LTree
  case class Var(x: String) extends LTree
  case class Arr(x: String, e: Etree) extends LTree
  case class Star(l: LTree) extends LTree
}

import scala.util.parsing.combinator.JavaTokenParsers

object ClikeminiArrayDec extends JavaTokenParsers with OpTree {
  // Parser
  def parse(source: String): List[Ctree] = 
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }
  def prog: Parser[List[Ctree]] = commlist
  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")
  def comm: Parser[Ctree] = 
    decl ^^ (Dec(_)) |      
    left~("="~>expr) ^^ { case l~e => Assign(l,e) } |
    "print"~>left ^^ { case l => Print(l) } |
    ("while"~>expr<~":")~(commlist<~"end") ^^ 
      { case e~cs => While(e,cs) }
  def decl: Parser[Dtree] =
    "int"~>ident ^^ (VarDec(_)) |
    "int"~>("["~>wholeNumber<~"]")~ident ^^ { case n~x => ArrDec(x,n) }
  def expr: Parser[Etree] = 
    wholeNumber ^^ (Num(_)) | 
    "("~>expr~op~expr<~")" ^^
      { case e1~"+"~e2 => Add(e1,e2)
        case e1~"-"~e2 => Sub(e1,e2) } |
    left ^^ (At(_)) |
    "&"~>left ^^ (Amph(_))
  def left: Parser[LTree] = 
    ident~("["~>expr<~"]") ^^ { case x~e => Arr(x,e) } |    
    ident ^^ (Var(_)) |
    "*"~>left ^^ (Star(_))
  def op: Parser[String] = "+" | "-"
  // Interpreter
  val memory = scala.collection.mutable.ArrayBuffer.empty[Int]
  var env = Map.empty[String,Int]
  def interpretPTREE(p: List[Ctree]): Unit = interpretCLIST(p)
  def interpretCLIST(cs: List[Ctree]): Unit = 
    for (c <- cs) yield interpretCTREE(c)
  def interpretCTREE(c: Ctree): Unit = c match {
    case Dec(d) => interpretDTREE(d)
    case Assign(l,e) => {
      val lval = interpretLTREE(l)
      val exprval = interpretETREE(e)
      memory(lval) = exprval
    }
    case Print(l) => {
      val loc = interpretLTREE(l)
      println(memory(loc))
    }
    case While(e,cs) => {
      val cond = interpretETREE(e)
      if (cond != 0) {
        interpretCLIST(cs)
        interpretCTREE(c)
      }
    }
  }
  def interpretDTREE(d: Dtree): Unit = {
    val newloc = memory.length
    d match {
      case VarDec(x) => { 
        memory += 0 // allocate a cell at the end of memory
        env += (x -> newloc) 
      }
      case ArrDec(x,n) => {
        for (i <- 1 to n.toInt) memory += 0 // 여기를 코드로 채우세요
        env += (x -> newloc)// 여기를 코드로 채우세요
      }
    }
  }
  def interpretETREE(e: Etree): Int = e match {
    case Num(n) => n.toInt
    case Add(e1,e2) => interpretETREE(e1) + interpretETREE(e2)
    case Sub(e1,e2) => interpretETREE(e1) - interpretETREE(e2)
    case At(l) => memory(interpretLTREE(l))
    case Amph(l) => interpretLTREE(l)
  }
  def interpretLTREE(l: LTree): Int = l match {
    case Var(x) => { 
      if (env contains x) env(x)
      else throw new Exception(x + "is not declared!")
    }
    case Arr(x,e) => {
      if (env contains x) env(x) + interpretETREE(e) // 여기를 코드로 채우세요
      else throw new Exception(x + "is not declared!") // 여기를 코드로 채우세요
    }
    case Star(l) => { // a pointer dereference
      val loc = interpretLTREE(l) // get a location number
      memory(loc) // dereference it and return the location therein
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
   