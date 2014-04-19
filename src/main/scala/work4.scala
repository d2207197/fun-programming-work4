package nthu.work4

import common._
import scala.math.pow

sealed trait Expr
case class Sum(x: Expr, y: Expr) extends Expr {
  override def toString(): String =
    "(" + x.toString() + "+" + y.toString() + ")"
}

case class Var(v: String) extends Expr {
  override def toString() = v
}

case class Const(a: Double) extends Expr {
  override def toString() = a.toString
}

case class Power(x: Expr, n: Const) extends Expr {
  override def toString() = x match {
    case Var(a) => a + "^" + n.toString
    case Const(a) => a.toString + "^" + n.toString
    case Sum(a,b) => x.toString + "^" + n.toString
    case _ => "(" + x.toString + ")" + "^" + n.toString
  }
}

case class Product(x: Expr, y: Expr) extends Expr {
  override def toString() =  x.toString + "*" + y.toString 
}


object Expr {

  def sameVar(x: Var, y: Var): Boolean = x.v == y.v

  def sameExpr(x: Expr, y: Expr): Boolean = (x, y) match {
    case (Var(a), Var(b)) => a == b
    case (Const(a), Const(b)) => a == b
    case (Power(ax, an), Power(bx, bn)) => sameExpr(ax, bx) && sameExpr(an, bn)
    case (Product(ax, ay), Product(bx, by)) => sameExpr(ax, bx) && sameExpr(ay, by)
    case (Sum(ax, ay), Sum(bx, by)) => sameExpr(ax, bx) && sameExpr(ay, by)
    case _ => false
  }

  def eval(e: Expr)(bind: String => Double): Double = e match {
    case Var(a) => bind(a)
    case Const(a) => a
    case Power(ax, an) => pow(eval(ax)(bind), eval(an)(bind))
    case Product(ax, ay) => eval(ax)(bind) * eval(ay)(bind)
    case Sum(ax, ay) => eval(ax)(bind) + eval(ay)(bind)
  }

  def deriv(e: Expr, v: Var): Expr = e match {
    case Var(a) => if (a == v.v) Const(1) else Const(0)
    case Const(a) => Const(0)
    case Power(ax, an) => Product(Product(an, Power(ax, Const(an.a-1))), deriv(ax, v))
    case Product(ax, ay) => Sum(Product(deriv(ax, v), ay), Product(ax, deriv(ay, v)))
    case Sum(ax, ay) => Sum(deriv(ax, v), deriv(ay, v))
  }

  def simplify(e: Expr): Expr = e match {
    case Var(_) | Const(_)=> e
    case Power(x, n) =>
      if (n.a == 1.0) simplify(x)
      else Power(simplify(x), n)
    case Product(ax, ay) =>
      val (ax_simp, ay_simp) = (simplify(ax), simplify(ay))
        (ax_simp, ay_simp) match {
        case (Const(a), Const(b)) => Const(a * b)

        case (Var(a), Var(b)) =>
          if (a == b) Power( Var(a), Const(2.0))
          else Product(Var(a), Var(b))

        case (Power(x1, n1), Power(x2, n2)) =>
          val (x1_simp, x2_simp) = (simplify(x1), simplify(x2))
          if (sameExpr(x1_simp, x2_simp)) Power(x1_simp, Const(n1.a + n2.a))
          else Product(Power(x1_simp, n1), Power(x2_simp, n2))

        case (_, Const(0)) => Const(0)
        case (Const(0), _) => Const(0)
        case (Const(a), Product(i, j)) =>
          (i, j) match {
            case (_ , Const(f)) => Product(Const(a * f), i)
            case (Const(f), _) => Product(Const(a * f), j)
            case _ => Product(Const(a), Product(i, j))
          }
        case (Product(i, j), Const(a)) => simplify(Product(Const(a), Product(i, j)))
        case (Product(i, j), Product(m, n)) => (i, m) match {
          case (Const(a), Const(b)) => Product(Const(a*b), Product(j, n))
          case _ => Product(Product(i, j), Product(m, n))
        }
        case _ => Product(simplify(ax), simplify(ay))
      }
    case Sum(ax, ay) => (simplify(ax), simplify(ay)) match {
      case (Const(a), Const(b)) => Const(a + b)
      case (Product(i1, i2), Product(l1, l2)) =>
        if (sameExpr(i2, l2)) simplify(Product(Sum(i1, l1), i2))
        else if (sameExpr(i2, l1)) simplify(Product(Sum(i1, l1), i2))
        else if (sameExpr(i1, l1)) simplify(Product(Sum(i1, l1), i1))
        else if (sameExpr(i1, l2)) simplify(Product(Sum(i1, l1), i1))
        else Sum(Product(i1, i2), Product(l1, l2))
      case (_, Const(0)) => simplify(ax)
      case (Const(0), _) => simplify(ay)
      case _ => Sum(simplify(ax), simplify(ay))
    }

  }

}

object Main extends App {
  import Expr._

  val e1: Expr = Sum(Power(Var("x"),Const(2)), Const(1))
  val e2: Expr = Product(Const(2), Var("x"))
  val e3: Expr = Sum(Product(Const(2),Var("x")), Product(Const(3),Var("x")))
  val e4: Expr = Product(Var("x"), Var("x"))
  val e5: Expr = Product(Power(Var("x"),Const(2)), Power(Var("x"),Const(3)))
  val e6: Expr = Sum(Power(Sum(Power(Var("x"),Const(2)),Product(Const(3),Var("x"))), Const(2)), Product(Const(4), Power(Var("x"), Const(2))))
  val e7: Expr = Product(Sum(Product(Const(2),Var("x")), Const(3)),Sum(Product(Const(3),Var("y")), Const(2)))
  val e8: Expr = Power(Sum(Product(Const(4),Power(Var("x"), Const(3))), Product(Const(3),Power(Var("y"), Const(2)))), Const(2))
  val e9: Expr = Power(Sum(Product(Const(4),Power(Var("x"), Const(3))), Product(Const(3),Power(Var("y"), Const(2)))), Const(2))


  println(e1);
  println(e2);
  println(e3);
  println(e4);
  println(e5);
  println(e6);
  println(e7);
  println(e8);



  
  println("-----\n evaluation")
  println(e1 + " = " + eval(e1)({case "x" => 5; case "y" => 2}))
  println(e2 + " = " + eval(e2)({case "x" => 5; case "y" => 2}))
  println(e3 + "->" + simplify(e3) + " = " + eval(e3)({case "x" => 5; case "y" => 2}))
  println(e4 + "->" + simplify(e4) + " = " + eval(e4)({case "x" => 5; case "y" => 2}))
  println(e5 + "->" + simplify(e5) + " = " + eval(e5)({case "x" => 5; case "y" => 2}))

  println(e6 + " = " + eval(e6)({case "x" => 5; case "y" => 2}))
  println(e7 + " = " + eval(e7)({case "x" => 5; case "y" => 2}))
  println(e8 + " = " + eval(e8)({case "x" => 3; case "y" => 2}))


  val de1 = deriv(e1,Var("x"))
  val de2 = deriv(e2,Var("x"))
  
  val de6 = deriv(e6,Var("x"))
  val de7 = deriv(e7,Var("x"))
  val de8 = deriv(e8,Var("x"))

  println("-----\n derivation and  evaluation")
  println(simplify(de1) + " = " + eval(de1)({case "x" => 5; case "y" => 2}))
  println(simplify(de2) + " = " + eval(de2)({case "x" => 5; case "y" => 2}))

  println(simplify(de6) + " = " + eval(de6)({case "x" => 5; case "y" => 2}))
  println(simplify(de7) + " = " + eval(de7)({case "x" => 5; case "y" => 2}))
  println(simplify(de8) + " = " + eval(de8)({case "x" => 3; case "y" => 2}))

}
