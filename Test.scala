// 2020/9/3
import scala.collection.mutable.ArrayBuffer




case class Token(tValue: String, tType: String){
    override def toString = s"$tValue: $tType"
}


object Lexer{
    def apply(str: String) = new Lexer(str)
}

class Lexer(text: String){
    var (pos, len) = (0, text.length)
    var cuCh: Option[Char] = if (text.isEmpty) None
                             else Some(text(pos))

    override def toString() = s"$text"

    val keywds = ArrayBuffer("fun", "var", "if", "eq?") ++
                 ArrayBuffer("set!", "mkBox", "unBox", "setBox!") ++
                 ArrayBuffer("cons", "car", "cdr", "sCar", "sCdr")
    val nmbs = ArrayBuffer((1 to 9).toArray: _*)
    val alphe = ArrayBuffer(('a' to 'z').toArray: _*) ++
                ArrayBuffer(('A' to 'Z').toArray: _*) ++
                ArrayBuffer('?', '!', '_')
    val lBracs = ArrayBuffer('(', '[', '{')
    val rBracs = ArrayBuffer(')', ']', '}')

    def advance(): Unit = {
        pos += 1
        if (pos < len)
            cuCh = Some(text(pos))
        else
            cuCh = None
    }

    def next(): Token = {
        while (!cuCh.isEmpty)
            if (nmbs.contains(cuCh.get.toInt -48))  //'1' -> 49 FUCK!
                return Token(digits(), "Nmb")
            else if (alphe.contains(cuCh.get))
                {val idff = idf(); return if(keywds.contains(idff)) Token(idff, idff)
                                          else                      Token(idff, "Idf")}
            else if (lBracs.contains(cuCh.get))
                return Token(bracs(), "lBracket")
            else if (rBracs.contains(cuCh.get))
                return Token(bracs(), "rBracket")
            else if (cuCh.get == '+')
                {advance(); return Token("+", "Add")}
            else if (cuCh.get == '=')
                {advance(); return Token("=", "Eq")}
            else
                advance()


        return Token("None", "END")
    }

    def digits(): String = {
        var result = ""
    while(!cuCh.isEmpty && nmbs.contains(cuCh.get.toInt -48)){
            result += cuCh.get
            advance()
        }

        return result
    }

    def idf(): String = {
        var result = ""
        while(!cuCh.isEmpty && alphe.contains(cuCh.get)){
            result += cuCh.get
            advance()
        }

        return result
    }

    def bracs(): String = {
        val brac = cuCh.get
        advance()

        return brac.toString
    }

    def op(): String = {
        val add = cuCh.get
        advance()

        return add.toString
    }

}



object T{
    def main(args: Array[String]) = {
        println("Done!")
    }
}



///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////



abstract class Expr
case class  Nmb(nmb: Int) extends Expr
case class  Idf(idf: String) extends Expr
case class  Add(l: Expr, r: Expr) extends Expr
case class  Eq(l: Expr, r: Expr) extends Expr
case class  Fun(paras: Array[String], body: Expr) extends Expr
case class  App(fun: Expr, args: Array[Expr]) extends Expr
case class  If(cond: Expr, texpr: Expr, fexpr: Expr) extends Expr
case class  Var(idf: String, body: Expr) extends Expr
case class  Begin(body: Array[Expr]) extends Expr
case class  Set(idf: String, value: Expr) extends Expr



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
            error("want a $tType, but given a $(cuTo.tType)")

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
                    case "set"              => set()
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
            paras += cuTo.tValue
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

    def set(): Set = {
        eat("set")
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




