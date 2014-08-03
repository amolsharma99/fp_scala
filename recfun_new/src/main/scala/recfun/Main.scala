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
    if(c==0 || r==0 || c==r)
    	1
    else
    	pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balanceParantheses(0, chars)
  
  def balanceParantheses(count: Int,chars: List[Char]): Boolean = {
    if(count < 0 || (chars.size==0 && count!=0))
      false
    else if(chars.size>0 && chars.head.toChar=='(')
      balanceParantheses(count+1, chars.tail)
    else if(chars.size>0 && chars.head.toChar==')')
      balanceParantheses(count-1,chars.tail)
    else if(chars.size>0)
      balanceParantheses(count, chars.tail)  
    else
      true  
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) {
      0
    } else if (money < 0) {
      0
    } else if (money == 0) {
      1
    } else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }
}
