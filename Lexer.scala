// 2020/9/3
import scala.collection.mutable.ArrayBuffer


object TestLexer{
    def main(args: Array[String]): Unit = {
        import scala.io.StdIn
        
        while (true) {
            print(">") //(fun (a b) (+ a (+ b 555)))
            val expr: String = StdIn.readLine()
            val lexer = Lexer(expr)
            var now = lexer.next()
            while (now.tType != "END"){
                print(now + "   ")
                now = lexer.next()
            }
            println()
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