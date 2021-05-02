package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println("--- Exercise 2 ---")
    println(balance("test(".toList))
    println(balance("test(".toList))
    println(balance("test()()".toList))
    println(balance("test(())".toList))
    println(balance("test(())(".toList))
    println(balance("test))((".toList))

    println("--- Exercise 3 ---")
    println(countChange(50, List(1, 10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def recursive(counter: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) counter == 0
      else {
        val char = chars.head
        if (char == ')' && counter == 0) false
        else {
          val newCounter: Int =
            if (char == '(') counter + 1
            else if (char == ')') counter - 1
            else counter
          recursive(newCounter, chars.tail)
        }
      }
    recursive(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recursive(amount: Int, coins: List[Int]): Int = {
      if (amount == 0) 1
      else if (amount < 0 || coins.length == 0) 0
      else recursive(amount, coins.tail) + recursive(amount - coins.head, coins)
    }

    recursive(money, coins.distinct)
  }
}
