package jsy.student

import jsy.lab1._

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {
  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /*
   * CSCI 3155: Lab 1
   * <caroline friedman>
   *
   * Partner: <adam ???>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y
  def testPlus() { assert(plus(1,1) == 2) }

  /* Exercises */

  /*
   *  absolute value of n
   *  n: double
   *  if n is greater than 0, return n
   *  if n is less than 0, return the opposite of n
   */
  def abs(n: Double): Double = { if (n >= 0) n else -n }

  /*
   *  xor, a ^ b
   *  a: boolean, b: boolean
   *  can only be 'a', or only be 'b', not both
   *  if (a = true, b = true) = false, else true
   *  vise-versa
   */
  def xor(a: Boolean, b: Boolean): Boolean = {
    if(a) {
      if(b) false else true
    } else {
      if(b) true else false
    }
  }

  /*
   *  recursion
   *  s: string, n: int
   *  return a string of s
   */
  def repeat(s: String, n: Int): String = {
    if(n < 0) throw new IllegalArgumentException
    else if(n == 0) ""
    else s + repeat(s, n - 1)
  }

  /*
   * square root single-step approximation
   * c: double, xn: double
   * returns the square root approximation of c
   */
  def sqrtStep(c: Double, xn: Double): Double = {
    xn - ((xn * xn) - c) / (2 * xn) // equation
  }

  /*
   * square root n-step approximation
   * c: double, x0: double, n: int
   */
  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    if(n > 0) sqrtN(c, sqrtStep(c, x0), n - 1) // recursion -> calls srqtN() function n-1 times
    else x0 // for 0 iterations, returns x0
  }

  /*
   * approximation within margin of error, epsilon
   * c: double, x0: double, epsilon: double
   */
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require(c >= 0) // no negative iterations
    if(epsilon <= 0) {
      throw new IllegalArgumentException
    } else if(abs(x0 * x0 - c) >= epsilon) {
      sqrtErr(c, sqrtStep(c, x0), epsilon) // recursion -> calls sqrtErr() until approximation < epsilon
    } else { x0 } // returns x0
  }

  /*
   * square root
   */
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) { 0 }
    else { sqrtErr(c, 1.0, 0.0001) }
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  /*
   * determines whether BST is valid
   * t: SearchTree
   */
  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true // base case
      case Node(l, d, r) => {
        if (d >= max || d < min) {
          false } // not a valid BST
        else {
          (check(l, min, d) && check(r, d, max)) // d max for left child node, d min for right child node
        }
      }
    }
    check(t, Int.MinValue, Int.MaxValue)
  }

  /*
   * insert a node into BST
   * t: SearchTree, n: int
   */
  def insert(t: SearchTree, n: Int): SearchTree = {
    t match {
      case Empty => Node(Empty, n, Empty) // in position, insert node
      case Node(l, d, r) => {
        if(n == d)
          Node(l, n, r)
        if(n < d)
          Node(insert(l, n), d, r) // compares node(s) values according to BST rules
        else
          Node(l, d, insert(r, n)) // left or right node depending on BST rules
      }
    }
  }

  /*
   * tuples
   * t: searchTree
   */
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty) // tree must not be empty
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d) // min, if left node empty
      case Node(l, d, r) => { // recursion
        val (l1, m) = deleteMin(l)
        (Node(l1, d, r), m) // rebuild parent nodes
      }
    }
  }

  /*
   * t: searchTree, n: int
   * BST
   */
  def delete(t: SearchTree, n: Int): SearchTree = {
    t match {
      case Empty => t
      case Node(Empty, d, Empty) => if(d == n) Empty else t
      case Node(Empty, d, r) => if (d == n) r else Node(Empty, d, delete(r, n))
      case Node(l, d, Empty) => if (d == n) l else Node(delete(l, n), d, Empty)
      case Node(l, d, r) => {
        if(n == d) {
          val (r1, v) = deleteMin(r)
          Node(l, v, r1)
        } else if (d > n) {
          Node(delete(l, n), d, r)
        } else {
          Node(l, d, delete(r, n))
        }
      }
    }
  }

  /* JavaScripty */

  def eval(e: Expr): Double = e match {
    case N(n) => n
    case Unary(Neg, e) => 0 - eval(e) // unary operation
    case Binary(bop, e1, e2) => bop match { // binary operation
      case Plus   => eval(e1) + eval(e2) // plus
      case Minus  => eval(e1) - eval(e2) // minus
      case Times  => eval(e1) * eval(e2) // times
      case Div    => eval(e1) / eval(e2) // div
    }
  }

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  // def eval(s: String): Double = eval(Parser.parse(s))



  /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}
