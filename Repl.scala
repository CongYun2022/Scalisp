//2020/9/3 


object REPL{
    val lBracs = Set('(', '[', '{')
    val rBracs = Set(')', ']', '}') 

    def main(args: Array[String]): Unit = 
        while (true)
            println("Done" + repl())

    def repl(): String = {
        import scala.io.StdIn

        var (expr, last, result) = ("  ", "  ", "")
        var (lBrac, rBrac) = (0, 0)
        print(">>")
        
        expr = StdIn.readLine()
        result += expr
        lBrac += expr.count(lBracs.contains(_))
        rBrac += expr.count(rBracs.contains(_))

        while((lBrac != rBrac) || last.trim() + expr.trim() == "\n\n") { 
            print("  ")                                 // for the >>

            last = expr
            expr = StdIn.readLine()
            result += expr
            lBrac += expr.count(lBracs.contains(_))
            rBrac += expr.count(rBracs.contains(_))
        }

        return "  " + result
    }



/*  so the <back> can`t be scaned, how to konw how many space in front of the expr?
    Console.in.read.toChar SO YESSS!!!

    def noFinish(expr: String): Boolean  = {
        var needInf = false
        val keywds = Set("fun", "var", "if", "begin")

        for (keywd <- keywds)
            needInf = needInf || expr.contains(keywd)
        
        return needInf
    }
*/
}



