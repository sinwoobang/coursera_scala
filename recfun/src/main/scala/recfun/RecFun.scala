package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def getPascal(y: Int, x: Int): Int = {
      if (y < 0 || x < y) 0
      else if (x <= 1) 1
      else getPascal(y - 1, x - 1) + getPascal(y, x - 1)
    }
    getPascal(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def isBalanced(i: Int, cnt: Int): Boolean = {
      if (i >= chars.length) cnt == 0
      else if (chars(i) == ')' && cnt == 0) false
      else if (chars(i) == ')') isBalanced(i+1, cnt-1)
      else if (chars(i) == '(') isBalanced(i+1, cnt+1)
      else isBalanced(i+1, cnt)
    }
    isBalanced(0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty) 0

    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else count(money, coins.tail) + count(money - coins.head, coins)
    }

    count(money, coins)
  }
}
