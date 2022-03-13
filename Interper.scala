// 2020/9/5 
import scala.collection.mutable.Map


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


type Location = Int
type Envrionment = Map[String, Location]
type Storage = Map[Location, Value]



abstract class Value
case class  Dig(nmb: Int) extends Value {
    override def toString = s"$nmb"
}
case class  Loc(loc: Location) extends Value {
    override def toString = s"Loc: $loc"
}
case class  Closure(paras: Array[String], body: Expr, env: Envrionment) extends Value {
    override def toString = s"(fun (${paras.mkString(" ")}) $body)"
}
case object Yes  extends Value
case object No   extends Value
case object Nil  extends Value




object Interp{
    def apply(expression: Expr): Value = {
/*      why not???  
        val globleEnv = Envrionment()
        val initStore = Storage() 
*/
        val globleEnv = Map[String, Location]()
        val initStore = Map[Location, Value]()
        val interper = new Interp(expression, initStore)
        interper.interp(expression, globleEnv)
    }
}

class Interp(expression: Expr, sto: Storage){
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



    def interp(expr: Expr, env: Envrionment): Value = 
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






