// 2020/9/5
import scala.collection.mutable.ArrayBuffer


abstract class Expr
case class  Nmb(nmb: Int) extends Expr {override def toString = s"$nmb"}
case class  Idf(idf: String) extends Expr  {override def toString = s"$idf"}
case class  Add(l: Expr, r: Expr) extends Expr {override def toString = s"(+ $l $r)"}
case class  Eq(l: Expr, r: Expr) extends Expr {override def toString = s"(= $l $r)"}
case class  Fun(paras: Array[String], body: Expr) extends Expr{
    override def toString = s"(fun (${paras.mkString(" ")}) $body)"
} // (fun (x y z) 1)
case class  App(fun: Expr, args: Array[Expr]) extends Expr {
    override def toString = s"($fun ${args.mkString(" ")})"
} // (func x)
case class  If(cond: Expr, texpr: Expr, fexpr: Expr) extends Expr {
    override def toString = s"(if $cond $texpr $fexpr)"
}
case class  Var(idf: String, value: Expr, body: Expr) extends Expr {
    override def toString = s"(var ($idf $value) $body)"
} //(var (x 555) x)
case class  Begin(body: Array[Expr]) extends Expr {
    override def toString = s"Begin(${body.mkString("\n")})"
} // (begin 1 2 3)
case class  Set(idf: String, value: Expr) extends Expr {
    override def toString = s"(set $idf $value)"
}

object Praser{
    def apply(lexer: Lexer) = new Praser(lexer)
}

class Praser(lexer: Lexer){
    class TokenTypeDisMatch(msg: String) extends Exception(msg){}

    var cuTo: Token = lexer.next()

    def eat(tType: String): Token = {
        if(cuTo.tType == tType)
            cuTo = lexer.next()
        else 
            error(s"want a $tType, but given a ${cuTo.tType}")

        return cuTo
    }

    // clear the right bracket, finish a ()
    def clear() = eat("rBracket")

    @throws(classOf[TokenTypeDisMatch])
    def error(msg: String) = throw new TokenTypeDisMatch(msg)


    def expr(): Expr = 
        cuTo.tType match {
            case "Nmb"      => nmb()
            case "Idf"      => idf()
            case "lBracket" => eat("lBracket").tType match {
                case "lBracket" | "Idf" => app()
                case "Add"              => add()
                case "Eq"               => eq()
                case _                      => cuTo.tValue match { //keyword
                    case "fun"              => fun()
                    case "if"               => `if`()
                    case "var"              => `var`()
                    case "set!"             => set()
                    case "begin"            => begin()
            }
        }
    }


    def nmb(): Nmb = {
        val result = Nmb(cuTo.tValue.toInt)
        eat("Nmb")

        return result
    }

    def idf(): Idf = {
        val result = Idf(cuTo.tValue)
        eat("Idf")

        return result
    }


    def app(): App = {
        val funDef = expr()
        val args = ArrayBuffer[Expr]()
        while (cuTo.tType != "rBracket")
            args += expr()
        clear()

        App(funDef, args.toArray)
    }

    def add(): Add = {
        eat("Add")
        val lexpr = expr()
        val rexpr = expr()
        clear()
        
        Add(lexpr, rexpr)
    }

    def eq(): Eq = {
        eat("Eq")
        val lexpr = expr()
        val rexpr = expr()
        clear()

        Eq(lexpr, rexpr)
    }

    def fun(): Fun = {
        eat("fun"); eat("lBracket") // (fun (<paras>) Expr)
        val paras = ArrayBuffer[String]()
        while (cuTo.tType != "rBracket")
            {paras += cuTo.tValue; eat("Idf")}
        clear()
        val body = expr()
        clear()

        Fun(paras.toArray, body)
    }

    def `if`(): If = {
        eat("if")
        val cond = expr()
        val texpr = expr()
        val fexpr = expr()
        clear()

        If(cond, texpr, fexpr)
    }

    def `var`(): Var = {
        eat("var")
        val idf = eat("lBracket").tValue
        eat("Idf")
        val value = expr()
        clear()
        val body = expr()
        clear()

        Var(idf, value, body)
    }

    def set(): Set = {
        eat("set!")
        val idf = cuTo.tValue
        eat("Idf")
        val value = expr()
        clear()

        Set(idf, value)
    }

    def begin(): Begin = {
        eat("begin")
        val body = ArrayBuffer[Expr]()

        while (cuTo.tType != "rBracket")
            body += expr()
        clear()

        Begin(body.toArray)
    }

}