package nthu.work4

import org.scalatest._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
  * Two options:
  *  1. run the "test" command in the SBT console
  *  2. right-click the file in eclipse and choose "Run As" -> "JUnit Test"
  */
  
@RunWith(classOf[JUnitRunner])
class Work4Suite extends FunSuite {

/*

  import nthu.work4.Expr._
  import nthu.work4._

  val e1: Expr = Sum(Power(Var("x"),Const(2)), Const(1))
  val e2: Expr = Product(Const(2), Var("x"))
  val e3: Expr = Sum(Product(Const(2),Var("x")), Product(Const(3),Var("x")))
  val e4: Expr = Product(Var("x"), Var("x"))
  val e5: Expr = Product(Power(Var("x"),Const(2)), Power(Var("x"),Const(3)))
  val e6: Expr = Sum(Power(Sum(Power(Var("x"),Const(2)),Product(Const(3),Var("x"))), Const(2)), Product(Const(4), Power(Var("x"), Const(2))))
  val e7: Expr = Product(Sum(Product(Const(2),Var("x")), Const(3)),Sum(Product(Const(3),Var("y")), Const(2)))
  val e8: Expr = Power(Sum(Product(Const(4),Power(Var("x"), Const(3))), Product(Const(3),Power(Var("y"), Const(2)))), Const(2))
  val e9: Expr = Power(Sum(Product(Const(4),Power(Var("x"), Const(3))), Product(Const(3),Power(Var("y"), Const(2)))), Const(2))

  val de1 = deriv(e1,Var("x"))
  val de2 = deriv(e2,Var("x"))
  
  val de6 = deriv(e6,Var("x"))
  val de7 = deriv(e7,Var("x"))
  val de8 = deriv(e8,Var("x"))

 simplify(de8)

 */

  import nthu.work4.Expr._

  val e1: Expr = Sum(Power(Var("x"),Const(2)), Const(1))
  val e2: Expr = Product(Const(2), Var("x"))
  val e3: Expr = Sum(Product(Const(2),Var("x")), Product(Const(3),Var("x")))
  val e4: Expr = Product(Var("x"), Var("x"))
  val e5: Expr = Product(Power(Var("x"),Const(2)), Power(Var("x"),Const(3)))
  val e6: Expr = Sum(Power(Sum(Power(Var("x"),Const(2)),Product(Const(3),Var("x"))), Const(2)), Product(Const(4), Power(Var("x"), Const(2))))
  val e7: Expr = Product(Sum(Product(Const(2),Var("x")), Const(3)),Sum(Product(Const(3),Var("y")), Const(2)))
  val e8: Expr = Power(Sum(Product(Const(4),Power(Var("x"), Const(3))), Product(Const(3),Power(Var("y"), Const(2)))), Const(2))
  val e9: Expr = Power(Sum(Product(Const(4),Power(Var("x"), Const(3))), Product(Const(3),Power(Var("y"), Const(2)))), Const(2))


  test("Case 1: sameVar OK") {
    assert(sameVar(Var("x"),Var("x")) == true)
    assert(sameVar(Var("x"),Var("y")) == false)
  }

  test("Case 2: sameExpr OK") {
    assert(sameExpr(e1, e9) == false)
    assert(sameExpr(e8, e9) == true)
  }

  test ("Case 3: evaluation OK") {
    assert(eval(e1)({case "x" => 5; case "y" => 2}) == 26.0)
  }
  test ("Case 4: evaluation OK") {
    assert(eval(e2)({case "x" => 5; case "y" => 2}) == 10.0)
  }
    test ("Case 5: evaluation OK") {
      assert(eval(e3)({case "x" => 5; case "y" => 2}) == 25.0)
    }
  test ("Case 6: evaluation OK") {
    assert(eval(e6)({case "x" => 5; case "y" => 2}) == 1700.0)
  }
    test ("Case 7: evaluation OK") {
      assert(eval(e7)({case "x" => 5; case "y" => 2}) == 104.0)
    }
  test ("Case 8: evaluation OK") {
    assert(eval(e8)({case "x" => 3; case "y" => 2}) == 14400.0)
  }

  val de1 = deriv(e1,Var("x"))
  val de2 = deriv(e2,Var("x"))
  
  val de6 = deriv(e6,Var("x"))
  val de7 = deriv(e7,Var("x"))
  val de8 = deriv(e8,Var("x"))
 
  test ("Case 9: derivation OK") {
    assert(eval(de1)({case "x" => 5; case "y" => 2}) == 10.0)
  }

  test ("Case 10: derivation OK") {
    assert(eval(de2)({case "x" => 5; case "y" => 2}) == 2.0)
  }

  test ("Case 11: derivation OK") {
    assert(eval(de6)({case "x" => 5; case "y" => 2}) == 1080.0)
  }

  test ("Case 12: derivation OK") {
    assert(eval(de7)({case "x" => 5; case "y" => 2}) == 16.0)
  }

  test ("Case 13: derivation OK") {
    assert(eval(de8)({case "x" => 3; case "y" => 2}) == 25920.0)
  }

  test ("Case 14: simplification OK") {
    assert(simplify(e3).toString == "5.0*x")
  }

  test ("Case 15: simplification OK") {
    assert(simplify(e4).toString == "x^2.0")
  }

  test ("Case 16: simplification OK") {
    assert(simplify(e5).toString == "x^5.0")
  }

  test ("Case 17: simplification OK") {
    assert(simplify(de7).toString == "2.0*(3.0*y+2.0)")
  }

  test ("Case 18: simplification OK") {
    assert(simplify(de8).toString == "24.0*(4.0*x^3.0+3.0*y^2.0)*x^2.0")
  }

  
}
