package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println(balance("((aa)a)".toCharArray.toList))
    println(balance("((aaa)".toCharArray.toList))
    println(balance("aaa)".toCharArray.toList))

    println(countChange(2, List(1, 2)))
    println(countChange(4, List(1, 2)))
    println(countChange(5, List(1, 2)))
    println(countChange(6, List(1, 2, 3)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (r == 0 || c <= 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_helper(given_list: List[Char], open_so_far: Int): Boolean = {
      if (open_so_far < 0) false
      else if (given_list.isEmpty) open_so_far == 0
      else {
        if (given_list.head == '(')
          balance_helper(given_list.tail, open_so_far + 1)
        else if (given_list.head == ')')
          balance_helper(given_list.tail, open_so_far - 1)
        else
          balance_helper(given_list.tail, open_so_far)
      }
    }
    balance_helper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val reverse_sorted = coins sortBy (-_)
    def count_helper(given_money: Int, given_coins: List[Int]): Int = {
      if (given_money == 0) 1
      else if (given_money < 0 || given_coins.isEmpty) 0
      else count_helper(given_money - given_coins.head, given_coins) + count_helper(given_money, given_coins.tail)
    }
    count_helper(money, reverse_sorted)
  }
}
