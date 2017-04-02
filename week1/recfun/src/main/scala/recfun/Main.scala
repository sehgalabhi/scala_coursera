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
    def pascal(c: Int, r: Int): Int = {

      def pascalInternal(c: Int, r: Int): Int = {
        if (root(c, r) || boundary(c, r)) {
          1
        } else if (invalid(c, r)) {
          0
        } else {
          pascalInternal(c - 1, r - 1) + pascalInternal(c, r - 1)
        }
      }

        def root(c: Int, r: Int) = {
          if ((c == 0) && (r == 0)) true else false
        }

        def boundary(c: Int, r: Int) = {
          if ((c == 0) || (c == r)) true else false
        }

        def invalid(c: Int, r: Int) = {
          if ((c < 0 || r < 0) || (c > r)) true else false
        }

      pascalInternal(c,r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceInternal(cOpen :Int, cClose:Int, chars: List[Char]):Boolean ={
        if(chars.isEmpty){
          if(cClose==cOpen){
            true
          } else{
            false
          }
        } else if(chars.head== '('){
          balanceInternal(cOpen+1, cClose, chars.tail)
        } else if(chars.head == ')') {
          if(cOpen ==0) {
            false
          } else {
            balanceInternal(cOpen-1, cClose, chars.tail)
          }
        } else{
          balanceInternal(cOpen, cClose, chars.tail)
        }
      }
      balanceInternal(0,0 , chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeInt(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (coins.isEmpty || money < 0) 0
        else countChangeInt(money - coins.head, coins) + countChangeInt(money, coins.tail)
      }

      countChangeInt(money, coins)
    }
  }