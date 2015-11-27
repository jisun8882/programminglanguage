package babyo

// heap : { Handle : Namespace }        -- Map[Handle,Map[String,Meaning]]
// where
// Namespace = { Identifier : Meaning } -- Map[String,Meaning]
// Meaning = Handle | Int               -- 

// Program P ::= CL
// CommandList CL ::= C | C ; CL
// Command C ::= L = E | if E : CL1 else CL2 end | print L
// Expression E ::= N | ( E1 + E2 ) | L | new { I,* }
//   where I,* means zero or more I-phrases, separated by comma
// LefthandSide L ::= I | L . I
// Numeral N ::= string of digits
// Variable I ::= strings of letters, not including keywords

trait OpTree {
  sealed abstract class Ctree
  case class Assign(l: Ltree, e: Etree) extends Ctree
  case class Cond(e: Etree, cs1: List[Ctree], cs2: List[Ctree]) extends Ctree
  case class Print(l: Ltree) extends Ctree

  sealed abstract class Etree
  case class Num(s: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class Deref(l: Ltree) extends Etree
  case class New(xs: List[String]) extends Etree
  
  sealed abstract class Ltree
  case class Id(x: String) extends Ltree
  case class Dot(l: Ltree, x: Ltree) extends Ltree
}

import scala.util.parsing.combinator.JavaTokenParsers

object BabyO extends JavaTokenParsers with OpTree {

  // Parser
  def parse(source: String): List[Ctree] = 
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }
  
  def prog: Parser[List[Ctree]] = commlist
  
  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")
  
  def comm: Parser[Ctree] = 
    lefts~("="~>expr) ^^ 
      { case l~e => Assign(l,e) } | 
    ("if"~>expr<~":")~(commlist<~"else")~(commlist<~"end") ^^ 
      { case e~cs1~cs2 => Cond(e,cs1,cs2) } |
    "print"~>lefts ^^ (Print(_))

  def expr: Parser[Etree] = 
    wholeNumber ^^ (Num(_)) | 
    "("~>expr~op~expr<~")" ^^
      { case e1~"+"~e2 => Add(e1,e2)
        case e1~"-"~e2 => Sub(e1,e2) } |
    "new"~>"{"~>rep1sep(ident,",")<~"}" ^^ (New(_)) |    
    lefts ^^ (Deref(_))
    
  def lefts: Parser[Ltree] = 
    ident~rep("."~>ident) ^^ 
      { case x~xs => val id: Ltree = Id(x)
                     val ids: List[Ltree] = for (y <- xs) yield Id(y)
                     (id /: ids) (Dot(_,_)) }

  def op: Parser[String] = "+" | "-"

  // Namespace Algebra
  // heap : { (Handle : Namespace)+ }
  // where Namespace = { Identifier : Meaning }
  //       Meaning = Handle | Integer
  //       LValue = Handle * Identifier

  // Heap
  sealed abstract class Rval
  case class Handle(loc: Int) extends Rval
  case class Value(n: Int) extends Rval
  case object Nil extends Rval

  var heap: Map[Handle, Map[String, Rval]] = Map()
  val ns = allocateNS()

  // Maintenance functions for the heap
  def allocateNS(): Handle = {
    val newhandle = Handle(heap.size)
    heap += (newhandle -> Map())
    newhandle 
  } 

  def lookup(lval: (Handle, String)): Rval = {
    val (handle, fieldname) = lval
    if ((heap contains handle) && (heap(handle) contains fieldname))
      heap(handle)(fieldname)
    else 
      throw new Exception("find error: " + handle + " does not exist in the heap")
  }

  def store(lval: (Handle, String), rval: Rval): Unit = {
    val (handle, fieldname) = lval
    if (heap contains handle) { 
      heap += (handle -> (heap(handle) + (fieldname -> rval)))
    }
    else // if the handle is not in the heap
      throw new Exception("bind error: " + handle + " does not exist in the heap")
  }

  // Interpreter
  
  def interpretPTREE(p: List[Ctree]): Unit = interpretCLIST(p)
  
  def interpretCLIST(cs: List[Ctree]): Unit = 
    for (c <- cs) yield interpretCTREE(c)
  
  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l,e) => {
      val lval = interpretLTREE(l)
      val rval = interpretETREE(e)
      store(lval,rval)
    }
    case Cond(e,cs1,cs2) =>
      interpretETREE(e) match {
        case Value(n) => if (n != 0) interpretCLIST(cs1)
                    else interpretCLIST(cs2)
        case _ => throw new Exception
      }
    case Print(l) => {
      println(lookup(interpretLTREE(l)))
    }
  }
  
  def interpretETREE(e: Etree): Rval = e match {
    case Num(n) => Value(n.toInt)
    case Add(e1,e2) => {
      val n1 = interpretETREE(e1) match {
                 case Value(n) => n
                 case _ => throw new Exception
               }
      val n2 = interpretETREE(e2) match {
                 case Value(n) => n
                 case _ => throw new Exception
               }
      Value(n1 + n2)
    }
    case Sub(e1,e2) => 
      {
      val n1 = interpretETREE(e1) match {
                 case Value(n) => n
                 case _ => throw new Exception
               }
      val n2 = interpretETREE(e2) match {
                 case Value(n) => n
                 case _ => throw new Exception
               }
      Value(n1 - n2)
    }
    case Deref(l) => {
      lookup(interpretLTREE(l))
    } 
    case New(fields) => {
      val newhandle = allocateNS()
      for (f <- fields) yield store((newhandle,f),Nil)
      newhandle
    } 
  }
  
  def interpretLTREE(left: Ltree): (Handle,String) = {
    left match {
      case Id(x) => (ns, x)
      case Dot(ls,Id(x)) => {
        val lval = interpretLTREE(ls)
        val handle = lookup(lval) match {
          case Handle(loc) => Handle(loc) 
          case _ => throw new Exception("This case will never be selected.")
        } 
        (handle,x) 
      }
      case _ => throw new Exception("This case will never be selected.")
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
      println("namespace : " + ns)
      println("heap : " + heap)
    }
    catch { case e: Exception => println(e) }
  }
}
   
