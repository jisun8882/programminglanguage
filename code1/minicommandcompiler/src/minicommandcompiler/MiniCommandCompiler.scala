package minicommandcompiler

// Mini-Command Language Compiler
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

// Abstract Syntax Tree
// PTREE ::= CLIST
// CLIST ::= List(CTREE)
// CTREE ::= Assign(VAR,ETREE) | Print(VAR) | While(ETREE,CLIST)
// ETREE ::= Number(S) | Var(S) | Add(ETREE,ETREE) | Sub(ETREE,ETREE)
// VAR is a sequence of letters

// Bytecode
// LOADNUM n       # integer n is pushed on top of the value stack
// LOAD v          # looks up value of var v and pushes it
// ADD             # pops top two integers, adds them, pushes answer
// SUBTRACT        # pops top two integers, subtracts them, pushes answer
// STORE v         # pops top integer and stores it in v’s memory cell
// PRINT v         # prints value in v’s memory cell
// IFZERO EXITLOOP # pops top int and if =0 exits innermost enclosing loop
// BEGINLOOP       # marks the code of a loop test+body
// ENDLOOP         # goto the corresponding BEGINLOOP

trait OpTree {
  sealed abstract class Etree
  case class Number(s: String) extends Etree
  case class Id(x: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  
  sealed abstract class Ctree
  case class Assign(x: String, e: Etree) extends Ctree
  case class Print(x: String) extends Ctree
  case class While(e: Etree, c: List[Ctree]) extends Ctree
}

import scala.util.parsing.combinator.JavaTokenParsers

object MiniCommandCompiler extends JavaTokenParsers with OpTree {
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
                         ident ^^ (Id(_)) |
                         "("~>expr~op~expr<~")" ^^
                         { case e1~"+"~e2 => Add(e1,e2)
                           case e1~"-"~e2 => Sub(e1,e2) }
  def op: Parser[String] = "+" | "-"

  // Compiler
  def compilePTREE(p: List[Ctree]): String =
    compileCLIST(p,List()) match { case (code,_) => code }
  def compileCLIST(cs: List[Ctree], symboltable: List[String])
                  : (String,List[String]) = cs match {
    case Nil => ("",symboltable)
    case c::cs =>
      val (code1,symboltable1) = compileCTREE(c,symboltable)
      val (code2,symboltable2) = compileCLIST(cs,symboltable1)
      (code1 + code2, symboltable2)
  }
  def compileCTREE(c: Ctree, symboltable: List[String]): (String, List[String]) =
    c match {
      case Assign(x,e) =>
        (compileETREE(e,symboltable) + "STORE " + x + "\n", x::symboltable)
      case Print(x) =>
        if (symboltable.contains(x)) ("PRINT " + x + "\n", symboltable)
        else throw new Exception("Error: " + x + " is undefined.")
      case While(e,cs) =>
        val exprcode = compileETREE(e,symboltable)
        val (bodycode,symboltable1) = compileCLIST(cs,symboltable)
        ("BEGINLOOP\n" + exprcode +
         "IFZERO EXITLOOP\n" + bodycode + "ENDLOOP\n",
         symboltable1)
  }
  def compileETREE(e: Etree, symboltable: List[String]): String = e match {
    case Number(s) => "LOADNUM " + s + "\n"
    case Id(x) =>
      if (symboltable.contains(x)) "LOAD " + x + "\n"
      else throw new Exception("Error: " + x + " is undefined.")
    case Add(e1,e2) =>  
      compileETREE(e1,symboltable) + compileETREE(e2,symboltable) + "ADD\n"
    case Sub(e1,e2) =>
      compileETREE(e1,symboltable) + compileETREE(e2,symboltable) + "SUBTRACT\n"
  }
  
  // Controller
  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      println("final namespace : ")
      print(compilePTREE(optree))
    }
    catch { case e: Exception => println(e) }
  }
}
