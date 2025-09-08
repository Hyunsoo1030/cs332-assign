package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def _balance(chars: List[Char], left: Int, right: Int): Boolean = {
      if (chars.isEmpty) left == right
      else if (chars.head == '(') _balance(chars.tail, left + 1, right)
      else if (chars.head == ')') (left > right) && _balance(chars.tail, left, right + 1)
      else _balance(chars.tail, left, right)
    }

    _balance(chars, 0, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def _count(money:Int, coins:List[Int]): Int = {
      if(money == 0) 1
      else if (money < 0) 0
      else coins match {
        case Nil => 0
        case h :: t => _count(money, t) + _count(money - h, coins)
      }
    }

    _count(money, coins)
  }
}
