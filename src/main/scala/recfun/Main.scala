package recfun

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
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def score(chars: List[Char], acc: Int = 0): Int =
        if (chars.isEmpty || acc < 0)
          acc
        else {
          val c = chars.head
          score(chars.tail, if (c == '(') acc + 1 else if (c == ')') acc - 1 else acc)
        }
      score(chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeCombinations(money: Int, coins: List[Int]): Int =
        if (money == 0)
          1
        else if (money < 0 || coins.isEmpty)
          0
        else
          countChangeCombinations(money - coins.head, coins) + countChangeCombinations(money, coins.tail)
      countChangeCombinations(money, coins)
    }
  }
