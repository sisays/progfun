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
    if(c == 0 || r == c) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parse(count: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) return true
      var increment = 0
      if (chars.head == '(') {
        increment = 1
      } else if (chars.head == ')'){
        increment = -1
      }
      val newCount = count + increment
      if(newCount < 0) false
      else parse(newCount, chars.tail)
    }
    parse(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
