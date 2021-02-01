package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println()
    println("Balance Parentheses Test")
    println(s"Checking ())( ${balance(List('(', ')', ')', '('))}")
    println(s"Checking ()(( ${balance(List('(', ')', '(', '('))}")

    println()
    println("Counting Change Test")
    println(s"Checking ways to provide 4 dollars with coints of 1 and 2 ${countChange(4, List(1,2))}")
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 1) 1
    else if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balancePatternMatching(chars: List[Char], count: Int = 0): Boolean = (chars, count) match {
      case (chars, 0) if chars.isEmpty => true
      case (chars, _) if chars.isEmpty => false
      case (chars, count) => chars.head match {
        case '(' => balancePatternMatching(chars.tail, count + 1)
        case ')' if count > 0 => balancePatternMatching(chars.tail, count - 1)
        case ')' => false
        case _ => balancePatternMatching(chars.tail, count)
      }
    }

    @tailrec
    def balance(chars: List[Char], balanceCount: Int): Boolean = {
      // If both parentheses match, balanceCount will be zero at the end.
      if (chars.isEmpty) balanceCount == 0
      else if (balanceCount < 0) false
        // For all other characters, just return original balanceCount
      else {
        val count =
          if (chars.head == '(') balanceCount + 1
          else if (chars.head == ')') balanceCount - 1
          else balanceCount
        balance(chars.tail, count)
      }
    }
    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (m, _) if m < 0 => 0
      case (_, coins) if coins.isEmpty => 0
      case (m, coins) => countChange(m - coins.head, coins) + countChange(m, coins.tail)
    }
  }
}
