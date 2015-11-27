

// heap : { Handle : Namespace }        -- Map[Handle,Map[String,Meaning]]
// where
// Namespace = { Identifier : Meaning } -- Map[String,Meaning]
// Meaning = Handle | Int               -- 

// Program P ::= CL
// CommandList CL ::= C | C ; CL
// Command C ::= L = E | if E : CL1 else CL2 end | print L
// Expression E ::= N | ( E1 + E2 ) | L | new { (I = E ),* }
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
  case class New(xs: List[(String,Etree)]) extends Etree
  
  sealed abstract class Ltree
  case class Id(x: String) extends Ltree
  case class Dot(l: Ltree, x: Ltree) extends Ltree
}

import scala.util.parsing.combinator.JavaTokenParsers

object Babyofield extends JavaTokenParsers with OpTree {

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
    "new"~>"{"~>rep1sep(ident~("="~>expr)^^{case i~e=>(i,e)},";")<~"}" ^^ (New(_)) |    
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
  // ns가 Handle에서 List로 변경되었습니다 stack을 List로 표현하였습니다
  var ns:List[Handle] = List(allocateNS())
  // Maintenance functions for the heap
  def allocateNS(): Handle = {
    val newhandle = Handle(heap.size)
    heap += (newhandle -> Map())
    newhandle 
  } 

  def lookup(lval: (Handle, String)): Rval = {
    val (handle, fieldname) = lval
    if ((heap contains handle))
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
  
  // 새로추가된 보조함수 find 입니다
  // 파라메터로 ns(namespace stack)과 필드 변수(String)을 받습니다
  // stack top에서 필드 변수를 찾은 후에 찾으면 top 핸들과 필드변수를 내주고 
  // 찾지 못하면 pop한후에 다시 찾아봅니다
  // stack안에 비웠을 경우 해당 변수를 못찾는 예외를 해줍니다
  
  def find(handles:List[Handle],x:String) :(Handle,String)={
    handles match{
      case h::t => 
        if (heap(h) contains x) (h,x)
        else find(t,x)      
      case _ => throw new Exception("not found: " + x )
    }
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
      // 아래에 있는 주석에 맞게 코드를 작성합니다 
      val (h,s) = interpretLTREE(l) // Deref안에 있는 Ltree를 계산합니다
      val t = List(h) ++ ns  // Ltree를 계산하고 나온 핸들값에다가 ns를 추가하여 새로운 변수에 저장하세요!
      // ns에 있는 핸들들에서 계산하고 나온 핸들이 포함되어 있으면 더해주지 않는 계산식을 만드는게 더 좋습니다(옵션)
      // 주의사항!! ns에 있는 값을 변경하면 안됩니다.  
      lookup(find(t,s)) // 새로 만든 변수와 Ltree에서 나온 string을 find 함수 파라메터로 넘겨준 후 
      // 결과를 lookup 함수 파라메터로 넘겨줍니다

    } 
    case New(fields) => {
      // 아래에 있는 주석에 맞게 코드를 작성합니다      
      val newhandle = allocateNS() // NS를 allcate 해주는 새로운 핸들변수를 만듭니다.
      ns = List(newhandle) ++ ns // ns에 새로 만든 핸들변수를 ns 앞에다가 추가하여 ns에 저장합니다.
      for ((i,e) <- fields) yield store((ns.head,i),interpretETREE(e)) // fields 타입에 맞게 계산해줍니다.(인터프리터가 어떻게 작동하는지 잘생각해보세요)
      ns = ns.tail // ns앞에 추가한 핸들값을 빼준 결과를 ns에 저장합니다.
      newhandle// 새로 만든 핸들변수를 내어줍니다.

    }
  }

  def interpretLTREE(left: Ltree): (Handle,String) = {
    left match {
      case Id(x) => (ns.head,x)
      case Dot(ls,Id(x)) => {
        val (han,lval) = interpretLTREE(ls)
        val (h,lx) = find(List(han)++ns,lval)
        val handle = lookup(h,lx) match {
          case Handle(loc) => Handle(loc) 
          case _ => throw new Exception("This case will never be selected.")
        } 
        if (heap(handle) contains x) (handle,x)
        else throw new Exception(x+" is not a member field "+lval)
        //x.y 일때 x의 핸들을 찾아왔는데 만약에 x핸들안에 y가 없으면 에러를 내주는 코드가 추가되었습니다
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

// 다음은 입력한 소스에 기대되는 결과 입니다.(혹시 버그를 발견하신분은 smhong@plasse.hanyang.ac.kr 로 알려주세요)
// 버그로 의심되는 부분도 알려주시면 감사합니다

// input : x = 2;y=new{a=x; b=a};z =new{ q=x;a =y.a}
// optree : List(Assign(Id(x),Num(2)), Assign(Id(y),New(List((a,Deref(Id(x))), (b,Deref(Id(a)))))), Assign(Id(z),New(List((q,Deref(Id(x))), (a,Deref(Dot(Id(y),Id(a))))))))
// namespace : List(Handle(0))
// heap : Map(Handle(0) -> Map(x -> Value(2), y -> Handle(1), z -> Handle(2)), Handle(1) -> Map(a -> Value(2), b -> Value(2)), Handle(2) -> Map(q -> Value(2), a -> Value(2)))

// input : x = 2;y=new{a=x; b=a};z =new{ q=x;a =y.x}
// optree : List(Assign(Id(x),Num(2)), Assign(Id(y),New(List((a,Deref(Id(x))), (b,Deref(Id(a)))))), Assign(Id(z),New(List((q,Deref(Id(x))), (a,Deref(Dot(Id(y),Id(x))))))))
// java.lang.Exception: x is not a member field y

// input : x = 2;y=new{a=x; b=a}
// optree : List(Assign(Id(x),Num(2)), Assign(Id(y),New(List((a,Deref(Id(x))), (b,Deref(Id(a)))))))
// namespace : List(Handle(0))
// heap : Map(Handle(0) -> Map(x -> Value(2), y -> Handle(1)), Handle(1) -> Map(a -> Value(2), b -> Value(2)))

