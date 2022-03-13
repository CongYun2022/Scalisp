// Playground
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

object Test{
    def main(args: Array[String]): Unit = {
        import scala.io.StdIn
        
        while (true) {
            print(">") // (+ (+ 1 2) 3)  (fun (a) a)  (begin 2 (func 4))  (fun (a b) (+ a (+ b 555)))
            var expr = StdIn.readLine()

            val lex1 = Lexer(expr)
            var now = lex1.next()
            while (now.tType != "END"){
                print(now + "   ")
                now = lex1.next()
            }
            println()

            val lexer = Lexer(expr)
            val praser = Praser(lexer)
            val exprIn = praser.expr()
            val value = Interp(exprIn)
            println(exprIn)
            println(value)
        }
    }
}


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

    val keywds = ArrayBuffer("fun", "var", "if", "begin") ++
                 ArrayBuffer("set!", "mkBox", "unBox", "setBox!") ++
                 ArrayBuffer("cons", "car", "cdr", "sCar", "sCdr")
    val nmbs = ArrayBuffer((1 to 9).toArray: _*)
    val alphe = ArrayBuffer(('a' to 'z').toArray: _*) ++
                ArrayBuffer(('A' to 'Z').toArray: _*) ++
                ArrayBuffer('?', '!', '_') ++ nmbs
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


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////






abstract class Value
case class  Dig(nmb: Int) extends Value {override def toString = s"$nmb"}
case class  Loc(loc: Int) extends Value {override def toString = s"Loc: $loc"}
case class  Closure(paras: Array[String], body: Expr, env: Map[String, Int]) extends Value {
    override def toString = s"(fun (${paras.mkString(" ")}) $body)"
}
case object Yes  extends Value
case object No   extends Value
case object Nil  extends Value




object Interp{
    def apply(expression: Expr): Value = {
/*      why not???  
        val globleEnv = Map[String, Int]()
        val initStore = Map[Int, Value]() 
*/
        val globleEnv = Map[String, Int]()
        val initStore = Map[Int, Value]()
        val interper = new Interp(expression, initStore)
        interper.interp(expression, globleEnv)
    }
}

class Interp(expression: Expr, sto: Map[Int, Value]){
    class InterpError(msg: String) extends Exception(msg){}

    @throws(classOf[InterpError])
    def error(msg: String): Value = {
        throw new InterpError(msg)
        Nil
    }


    var x = 0
    def newLoc() = {x+=1; x}

    def add(lv: Value, rv: Value): Value = 
        Dig(lv.asInstanceOf[Dig].nmb + rv.asInstanceOf[Dig].nmb)



    def interp(expr: Expr, env: Map[String, Int]): Value = 
        expr match {
            case Nmb(nmb) => Dig(nmb)
            case Idf(idf) => {val value = sto.get(env.get(idf).getOrElse(-1))
                              value match {
                                  case None       => error(s"No such idf: $idf")
                                  case Some(v)    => v.asInstanceOf[Value]}} 
            case Add(le, re)    => {val lv = interp(le, env)
                                    val rv = interp(re, env)
                                    add(lv, rv)}
            case Eq(le, re)     => {val lv = interp(le, env)
                                    val rv = interp(re, env)
                                    if (lv == rv) Yes else No}
            case Fun(paras, body)   => Closure(paras, body, env)
            case App(fun, args)     => {val funDef = interp(fun, env)
                                        val argsVal = for (arg <- args) yield interp(arg, env)
                                        for (pa <- funDef.asInstanceOf[Closure].paras.zip(argsVal)){
                                            val ll = newLoc()
                                            env += (pa._1 -> ll)
                                            sto += (ll -> pa._2)}
                                        interp(funDef.asInstanceOf[Closure].body, env)}
            case If(ts, te, fe)  => interp(ts, env) match {
                                        case Yes => interp(te, env)
                                        case No  => interp(fe, env)
                                        case _   => error("If expr only can get Bool")}
            case Set(idf, vale)         => {val value = interp(vale, env)
                                            sto(env(idf)) = value
                                            value}
            case Var(idf, vale, body)   => {val value = interp(vale, env)
                                            val ll = newLoc()
                                            env += (idf -> ll)
                                            sto += (ll -> value)
                                            interp(body, env)}
            case Begin(body)            => (for (ex <- body) yield interp(ex, env)).last
        }

}



