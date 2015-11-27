import scala.util.parsing.combinator._
import java.io.FileReader

object JSON extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] =
    "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _) 
  
  def arr: Parser[List[Any]] =
    "["~> repsep(value, ",") <~"]" ^^ (List() ++ _)
    
  def member: Parser[(String, Any)] =
    stringLiteral~":"~value ^^
    { case name~":"~value => (name,value) }
    
  def value: Parser[Any] = (
    obj |
    arr |
    stringLiteral |
    floatingPointNumber ^^ (_.toDouble) |
    "null" ^^ (x => null) |
    "true" ^^ (x => true) |
    "false" ^^ (x => false)
  )

  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(value, reader))
  }
}