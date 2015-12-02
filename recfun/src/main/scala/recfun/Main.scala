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
  def pascal(c: Int, r: Int): Int = 
    if ((c==0) && (r==0)) 1 else
      if ((c<0) || (c>r)) 0 else 
        pascal(c-1, r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
        def balance_count(bracket_count: Int, chars: List[Char]): Boolean = {
          if (bracket_count <0) false else
          if (chars.isEmpty)
            if (bracket_count == 0) true else false 
          else {
            val first = chars.head
            if (first == '(') balance_count(bracket_count + 1, chars.tail) 
            else
              if (first == ')') balance_count(bracket_count - 1, chars.tail) 
              else balance_count(bracket_count, chars.tail)
          }
        }
        
        balance_count(0, chars)
          
      }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
