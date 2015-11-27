// P ::= D ; C
// E ::= N | ( E1 + E2 ) | L | (E1 == E2) | not E | new T | nil
// C ::= L = E | if E { C1 } else { C2 } | print L | C1 ; C2 | while E { C }
// D :: var I = E | D1 ; D2
// T :: struct D end | array[N] of E
// L ::= I | L . I | L[E]
// N ::= string of digits
// I ::= strings of letters, not including keywords

trait OpTree4 {
  sealed abstract class Ctree
  case class Assign(l: Ltree, e: Etree) extends Ctree
  case class Cond(e: Etree, cs1: Ctree, cs2: Ctree) extends Ctree
  case class While(e: Etree, cs1: Ctree) extends Ctree
  case class Print(l: Ltree) extends Ctree
  case class Seq(cl: List[Ctree]) extends Ctree
  case class Call_proc(l: Ltree) extends Ctree
  case class Call_proc_para(l: Ltree, e: List[Etree]) extends Ctree

  sealed abstract class Etree
  case class Num(s: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class Deref(l: Ltree) extends Etree
  case class New(xs: Ttree) extends Etree
  case class Not(e:Etree) extends Etree
  case class Bop(e:Etree,e1:Etree) extends Etree
  case class Nil_() extends Etree

  sealed abstract class Ltree
  case class Id(x: String) extends Ltree
  case class Dot(l: Ltree, x: Ltree) extends Ltree
  case class Arr(l: Ltree, e: Etree) extends Ltree

  sealed abstract class Dtree
  case class Dec(x: String, e: Etree) extends Dtree
  case class Dseq(dl: List[Dtree]) extends Dtree
  case class Proc(i:String, c:Ctree) extends Dtree
  case class Cl(i:String, t: Ttree) extends Dtree
  case class Mod(i:String, D: Dtree) extends Dtree
  case class Call_mod(l: Ltree) extends Dtree
  case class Proc_para(i:String, l:List[String], c:Ctree) extends Dtree
  case class Cl_para(i:String, l:List[String], t:Ttree) extends Dtree
  case class Sub_cl(i1:String, i2:String , t:Ttree) extends Dtree

  sealed abstract class Ttree
  case class St(d: Dtree) extends Ttree
  case class Ar(n: Int, e: Etree) extends Ttree
  case class Call_class (l:Ltree) extends Ttree
  case class Call_class_para (l:Ltree, e:List[Etree]) extends Ttree
  case class Mixins(t1:Ttree, t2:Ttree) extends Ttree
}

import scala.util.parsing.combinator.JavaTokenParsers

object oocore4 extends JavaTokenParsers with OpTree4 {

  // Parser
  def parse(source: String): (Dtree, Ctree) =
    parseAll(prog, source) match {
      case Success(optree, _) => optree
      case _ => throw new Exception("Parse error!")
    }

  def prog: Parser[(Dtree, Ctree)] =
    (defns <~ ";") ~ coms ^^ { case d ~ c => (d, c) } |
      defns ^^ { case d => (d, Seq(List())) }

  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")

  def coms: Parser[Ctree] = rep1sep(comm, ";") ^^ { case cl => Seq(cl) }
  def comm: Parser[Ctree] =
    lefts ~ ("=" ~> expr) ^^ { case l ~ e => Assign(l, e) } |
      ("if" ~> expr <~ "{") ~ (coms<~"}" <~ "else") ~ ("{"~>coms <~ "}") ^^ { case e ~ cs1 ~ cs2 => Cond(e, cs1, cs2) } |
      ("while" ~> expr <~ "{") ~ coms <~ "}" ^^ { case e ~ cs1 => While(e, cs1) } |
      "print" ~> lefts ^^ (Print(_)) |
       lefts <~ "()" ^^ { case l => Call_proc(l)}   |
       (lefts <~ "(") ~ (repsep(expr,",") <~ ")") ^^ { case l ~ e => Call_proc_para(l,e)}
    

  def defns: Parser[Dtree] = rep1sep(defn, ";") ^^ { case dl => Dseq(dl) }
  def defn: Parser[Dtree] =
    ("var" ~> ident) ~ ("=" ~> expr) ^^ { case i ~ e => Dec(i, e) } |
    ("proc" ~> ident <~ "()") ~ ("{" ~> coms <~ "}") ^^ { case i ~ c => Proc(i,c) } |
    ("class" ~> ident) ~ ("=" ~> templ) ^^ { case i ~ t => Cl(i,t)} |
    ("module" ~> ident) ~ ("=" ~> defns <~ "end") ^^ { case i ~ d => Mod(i,d) } |
    ("import" ~> lefts) ^^ { case l => Call_mod(l)} |
    ("proc" ~> ident) ~ ( "("~> repsep(ident,",") <~ ")") ~ ("{" ~> coms <~ "}") ^^ { case i ~ l ~ c => Proc_para(i,l,c) } |
    ("class" ~> ident) ~ ( "(" ~> repsep(ident,",") <~ ")") ~ ("=" ~> templ) ^^ { case i ~ l ~ t => Cl_para(i,l,t)} |
    ("class" ~> ident) ~ ( "extends" ~> ident ) ~ ("=" ~> templ) ^^ { case i1 ~ i2 ~ t => Sub_cl(i1,i2,t)}

  def expr: Parser[Etree] =
    wholeNumber ^^ (Num(_)) |
    "not"~> expr ^^ (Not(_)) |
    ("(" ~> expr <~ "==") ~ expr <~ ")" ^^ { case e1~e2 => Bop(e1,e2)} |
      "(" ~> expr ~ op ~ expr <~ ")" ^^
      {
        case e1 ~ "+" ~ e2 => Add(e1, e2)
        case e1 ~ "-" ~ e2 => Sub(e1, e2)
      } |
      "new" ~> templ ^^ (New(_)) |
      lefts ^^ (Deref(_))

  
  def templ: Parser[Ttree] =
    "struct" ~> defns <~ "end" ^^ (St(_)) |
      ("array" ~> "[" ~> wholeNumber <~ "]") ~ ("of" ~> expr) ^^
      { case n ~ e => Ar(n.toInt, e) } |
      ("(" ~> templ) ~ ("+" ~> templ <~ ")") ^^ { case t1 ~ t2 => Mixins(t1,t2)} |
      (lefts <~ "(") ~ (repsep(expr,",") <~ ")") ^^ { case l ~ e => Call_class_para(l,e)} |
      lefts ^^ { case l => Call_class(l)}

  def le: Parser[Ltree] =
    ident ~ rep("[" ~> expr <~ "]") ^^ {
      case l ~ el =>
        val id: Ltree = Id(l)
        (id /: el)(Arr(_, _))
    } |
      ident ^^ (Id(_))

  def lefts: Parser[Ltree] =
    rep1sep(le, ".") ^^
    {
      case h:: List() => h
      case v :: lst => (v /: lst) (Dot(_,_))
      case List() => throw new Exception("empty lhs")
    }

  def op: Parser[String] = "+" | "-"

  sealed abstract class Rval
  case class Handle(loc: Int) extends Rval
  case class Value(n: Int) extends Rval
  case object Nil extends Rval
  case class Com_proc(e : Ctree) extends Rval
  case class Com_class(t : Ttree) extends Rval
  case class Com_mod(d : Dtree) extends Rval
  case class Call_para(l :List[String]) extends Rval
  case class Com_sub_class(t : Ttree) extends Rval
  
  sealed abstract class Mem
  case class Namespace(c: Map[String, Rval]) extends Mem
  case class M_Array(n: scala.collection.mutable.ArrayBuffer[Rval]) extends Mem

  var heap: Map[Handle, Mem] = Map()

  var ns: List[Handle] = List()
  ns = List(allocateNS())

  def allocateNS(): Handle = {
    val newhandle = Handle(heap.size)
    val p = if (ns == List()) Handle(-1) else ns.head
    heap += (newhandle -> Namespace(Map("parents" -> p)))
    newhandle
  }

  def heap_of_loc(m: Mem): Map[String, Rval] = m match {
    case Namespace(l) => l
    case _ => throw new Exception("not Namespace")
  }

  def lookup(lval: (Handle, String, Int)): Rval = {
    val (handle, fieldname, i) = lval
    heap(handle) match {
      case Namespace(m) =>
        if ((heap contains handle))
          heap_of_loc(heap(handle))(fieldname)
        else
          throw new Exception("lookup error: "+handle+ " Not contains "+ fieldname)
      case M_Array(a) => a(i)
    }
  }

  def store(lval: (Handle, String, Int), rval: Rval): Unit = {
    val (handle, fieldname, i) = lval
    heap(handle) match {
      case Namespace(m) =>
        if (heap contains handle) {
          heap += (handle -> Namespace((heap_of_loc(heap(handle)) + (fieldname -> rval))))
        } else // if the handle is not in the heap
          throw new Exception("bind error: " + handle + " does not exist in the heap")
      case M_Array(a) =>{
          a(i) = rval
          heap += (handle -> M_Array(a))
      }
    }
  }

  def find(lval: (Handle, String)): Handle = {
    val (han, x) = lval
    if (heap_of_loc(heap(han)) contains x) han
    else {
      heap_of_loc(heap(han))("parents") match {
        case Handle(h) =>
          if (Handle(h) == Handle(-1)) throw new Exception("find error not found " + x)
          else find((Handle(h), x))
        case _ => throw new Exception("This case will never be selected.(find parents)")
      }
    }
  }
  // Interpreter

  def interpretPTREE(p: (Dtree, Ctree)): Unit = {
    val (d, c) = p
    interpretDTREE(d)
    interpretCTREE(c)
  }

  def interpretDTREE(d: Dtree): Unit = d match {
    case Dec(x, e) => {
      store((ns.head, x, -1), interpretETREE(e))
    }
    case Dseq(ds) => for (d <- ds) interpretDTREE(d)
    case Proc(i,e) => {
      val handle = allocateNS();
      store((ns.head,i,-1), handle)
      store((handle, "callproc" , -1), Com_proc(e))
    }
    case Proc_para(i,l,e) => {
      val handle = allocateNS();
      store((ns.head,i,-1),handle)
      store((handle, "callpara",-1), Call_para(l))
      store((handle, "callproc",-1), Com_proc(e))      
    }  
    case Cl(i,t) => {
      val handle = allocateNS();
      store((ns.head,i,-1), handle)
      store((handle, "callpara",-1), Call_para(List()))      
      store((handle, "callclass" , -1), Com_class(t))
    }
    case Cl_para(i,l,t) => {
      val handle = allocateNS();
      store((ns.head,i,-1), handle)
      store((handle, "callpara",-1), Call_para(l))
      store((handle, "callclass" , -1), Com_class(t))
    }
    case Sub_cl(i1,i2,t) =>{
      val handle = allocateNS();
      store((ns.head,i1,-1),handle)
      store((handle,"parent",-1), lookup(ns.head,i2,-1))
      store((handle,"callclass",-1), Com_sub_class(t))
    }
    case Mod(i,d) => {
      val handle = allocateNS();
      store((ns.head,i,-1),handle)
      store((handle,"callmod",-1),Com_mod(d))
    }
    case Call_mod(l) => {
      val (n_handle,n_id,_) = interpretLTREE(l,ns.head)
      val a = lookup(n_handle,n_id,-1) match
      {
        case Handle(a) => 
          heap_of_loc(heap(Handle(a)))("callmod") match{
            case Com_mod(d) => {
              interpretDTREE(d)
            }
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
      
    }
  }
  
  def interpretTTREE(t: Ttree): Unit = t match {
    case St(d) => interpretDTREE(d)
    case Ar(n,e) => {
      val arr = scala.collection.mutable.ArrayBuffer[Rval]()
      for(i <- 0 to n-1) arr += interpretETREE(e)
      heap += (ns.head -> M_Array(arr))
    }
    case Call_class(l) => {
      val (n_handle,n_id,_) = interpretLTREE(l,ns.head)
      lookup(n_handle,n_id,-1) match
      {
        case Handle(a) => 
          heap_of_loc(heap(Handle(a)))("callclass") match{
            case Com_class(t) => {
              interpretTTREE(t)
            }
            case Com_sub_class(t) => {
              lookup(Handle(a),"parent",-1) match {
                case Handle(a) => 
                  heap_of_loc(heap(Handle(a)))("callclass") match{
                    case Com_class(t) => {
                    interpretTTREE(t)
                    }
                  }
              }
            }
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
    case Call_class_para(l,e) => {
      val (n_handle,n_id,_) = interpretLTREE(l,ns.head)
      lookup(n_handle,n_id,-1) match
      {
        case Handle(a) => {
            lookup(Handle(a),"callpara",-1) match {
              case Call_para(pl) => {
                var el = e
                for (p <- pl){
                  store((ns.head,p,-1),interpretETREE(el.head))
                  el = el.tail
                }
                lookup(Handle(a),"callclass",-1) match {
                  case Com_class(t) =>
                    interpretTTREE(t)
                  case _ => throw new Exception
                }
              }
              case _ => throw new Exception
            }
          }
        case _ => throw new Exception
      }
    }
    case Mixins(t1,t2) => {
      interpretTTREE(t1)
      interpretTTREE(t2)
    }
  }
  
  def interpretCLIST(cs: List[Ctree]): Unit =
    for (c <- cs) yield interpretCTREE(c)

  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l, e) => {
      store(interpretLTREE(l,ns.head), interpretETREE(e))
    }
    case Cond(e, cs1, cs2) =>
      interpretETREE(e) match {
        case Value(n) => if (n != 0) interpretCTREE(cs1)
        else interpretCTREE(cs2)
        case _ => throw new Exception
      }
    case While(e, cs1) =>
      interpretETREE(e) match {
        case Value(n) =>
          if (n != 0) {
            interpretCTREE(cs1)
            interpretCTREE(c)
          }
        case _ => throw new Exception
      }
    case Call_proc(l) => {
      val (n_handle,n_id,_) = interpretLTREE(l,ns.head)
      val a = lookup(n_handle,n_id,-1) match
      {
        case Handle(a) => 
          heap_of_loc(heap(Handle(a)))("callproc") match{
            case Com_proc(c) => {
              ns = Handle(a) :: ns
              interpretCTREE(c)
              ns = ns.tail
            }
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    } 
    case Call_proc_para(l,e) => {
      val nh = allocateNS()
      val (n_handle,n_id,_) = interpretLTREE(l,ns.head)
      lookup(n_handle,n_id,-1) match
      {
        case Handle(a) => {
            lookup(Handle(a),"callpara",-1) match {
              case Call_para(pl) => {
                var el = e
                for (p <- pl){
                  store((nh,p,-1),interpretETREE(el.head))
                  el = el.tail
                }
                // ++ @
                ns = List(nh) ++ ns
                lookup(Handle(a),"callproc",-1) match {
                  case Com_proc(c) =>
                    interpretCTREE(c)
                    ns = ns.tail
                  case _ => throw new Exception
                }
              }
              case _ => throw new Exception
            }
          }
        case _ => throw new Exception
      }
    }

    case Print(l) => {
      println(lookup(interpretLTREE(l,ns.head)))
    }
    case Seq(cs) => for (c <- cs) yield interpretCTREE(c)
  }

  def interpretETREE(e: Etree): Rval = e match {
    case Nil_() => Nil
    case Bop(e1,e2)=>{
      if (interpretETREE(e1)==interpretETREE(e2)) Value(1) else Value(0)
    }
    case Not(e) =>{
      interpretETREE(e) match {
        case Value(n) =>
          if (n==1) Value(0) else Value(1)
        case _ => throw new Exception
      }
    }
    case Num(n) => Value(n.toInt)
    case Add(e1, e2) => {
      val n1 = interpretETREE(e1) match {
        case Value(n) => n
        case _ => throw new Exception (e1+" is not value")
      }
      val n2 = interpretETREE(e2) match {
        case Value(n) => n
        case _ => throw new Exception (e2+" is not value")
      }
      Value(n1 + n2)
    }
    case Sub(e1, e2) =>
      {
        val n1 = interpretETREE(e1) match {
          case Value(n) => n
          case _ => throw new Exception (e1+" is not value")
        }
        val n2 = interpretETREE(e2) match {
          case Value(n) => n
          case _ => throw new Exception (e2+" is not value")
        }
        Value(n1 - n2)
      }
    case Deref(l) => {
      lookup(interpretLTREE(l,ns.head))
    }
    case New(t) => {
            val newhandle = allocateNS() // NS를 allcate 해주는 새로운 핸들변수를 만듭니다.
            ns = List(newhandle) ++ ns // ns에 새로 만든 핸들변수를 ns 앞에다가 추가하여 ns에 저장합니다.
            interpretTTREE(t)
            ns = ns.tail // ns앞에 추가한 핸들값을 빼준 결과를 ns에 저장합니다.
            newhandle// 새로 만든 핸들변수를 내어줍니다.
    }
  }

  def interpretLTREE(left: Ltree,han1:Handle): (Handle, String, Int) = {
    left match {
      case Id(x) => {
        (find(han1, x), x, -1)
      }
      case Dot(ls, l1) => {
        val han = lookup(interpretLTREE(ls,han1)) match {
          case Handle(s) => Handle(s)
          case _ => throw new Exception("It's not value")
        }
        l1 match {
          case Id(x) => if(heap_of_loc(heap(han)) contains x) (han,x,-1)
                        else throw new Exception("Not member "+x)
          case _ => interpretLTREE(l1,han)
        }
        
      }
      case Arr(l, e) => {
        val (han, r, i) = interpretLTREE(l, han1)
        lookup(han, r, i) match {
          case Handle(s) =>  {
            interpretETREE(e) match {
              case Value(n) => (Handle(s), r, n)
              case _ => throw new Exception("It's not value")
            }
          }
          case _ => throw new Exception("It's not value") 
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
    //val source = "class CLK = struct var time = 0;proc tick() { time = (time + 1) };proc display() { print time };proc reset() { time = 0 } end; var clock = new CLK;clock.tick();clock.display();clock.reset();clock.display()"

//val source = "class CLK = struct var time = 0;proc tick() { time = (time + 1) };proc display() { print time };proc reset() { time = 0 }end;var clock = new CLK;var clock2 = new CLK;clock.tick();clock.display();clock.reset();clock.display();clock2.tick();clock2.display();clock2.reset();clock2.display()"

//val source = "var x = 3;class CLK = struct var time = 0;proc tick() { x = ( x + 2) ; time = (x+ 1) };proc display() { print time };proc reset() { time = 0 } end;var clock = new CLK;clock.tick();clock.display();clock.reset();clock.display();clock.tick();clock.display();clock.reset();clock.display()"

//val source = "module M = var x = 0; class C = struct var a=0 end; var y = new array[10] of new C; proc initialize(){ x = 9; while x { y[x].a = x; x = (x - 1) } } end; initialize()"

//val source = "module M = var x = 0; class C = struct var a=0 end; var y = new array[10] of new C; proc initialize(){ x = 9; while x { y[x].a = x; x = (x - 1) }; x = 100 } end; import M; initialize(); print x"

//val source = "module M = var x = 0; class C = struct var a=0 end; var y = new array[10] of new C; proc initialize(){ x = 9; while x { y[x].a = x; x = (x - 1) }; x = 100 } end; import M; var z = new C; initialize(); z.a = (x+ y[5].a); print z.a"

//val source = "var x = 10; proc sum(a1,a2){ print a1; print a2; x = (a1+a2) }; sum(10,20); print x"

//val source = "var x = 0; class C = struct var a=0 end; var y = new array[10] of new C; proc sum(a1,a2){ print a1; print a2; x = (a1+a2) }; proc initialize(d){ sum(d,2); print x; while x { y[x].a = x; x = (x - 1) }; x = 100 }; var z = new C; initialize(5); z.a = (x+ y[5].a); print z.a; z.a = (x+ y[8].a); print z.a"

//val source = "class CLK(x) = struct var time = x; proc tick() { time = (time + 1) }; proc display() { print time }; proc reset() { time = 0 } end; var clock = new CLK(10); clock.tick(); clock.display()"

//val source = "class CLK = struct var time = 0; proc tick() { time = (time + 1) }; proc display() { print time }; proc reset() { time = 0 } end; class CLK2 extends CLK = struct proc tick() { time = (time + 1) }; proc display() { print time }; proc reset() { time = 0 } end; var clock = new CLK2; clock.tick(); clock.display()"

//val source = "class CLK = struct var time = 0; proc tick() { time = (time + 1) }; proc display() { print time }; proc reset() { time = 0 } end; class CLK2 extends CLK = struct proc tick() { time = (time + 1) }; proc display() { print time } end; var clock = new CLK2; clock.tick(); clock.display(); clock.tick(); clock.display()"

//val source = "class CLK = struct var time = 0; proc tick() { time = (time + 1) }; proc display() { print time }; proc reset() { time = 0 } end; class CLK2 extends CLK = struct var time = 10; proc tick() { time = (time + 1) }; proc display() { print time } end; var clock = new CLK2; clock.tick(); clock.display(); clock.tick(); clock.display()"

//val source = "var p = new (struct var x = 10; var y = 20 end + struct var color = new array[3] of 0 end); p.x = 0; p.color[1] = 255; print p.color[1]; print p.y"

//val source = "class Point = struct var x = 10; var y = 20; proc paint() { print x; print y } end; class Color = struct var color = new array[3] of 0; proc paint() { print color[0];print color[1] } end; class ColoredPoint = (Point + Color); var p = new ColoredPoint; p.x = 0; p.color[1] = 255; p.paint()"

val source = "class Point = struct var x = 10; var y = 20; proc paint() { print x; print y } end; class Color = struct var color = new array[3] of 0; proc paint() { print color[0];print color[1] } end; class ColoredPoint = (Point + Color); var p = new ColoredPoint; p.color[0] = p.x; p.color[1] = 255; p.paint()"
      
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      interpretPTREE(optree)
      println("namespace : " + ns)
      println("heap : " + heap)
    } catch { case e: Exception => println(e) }
  }
}

