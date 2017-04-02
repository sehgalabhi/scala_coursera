def countChange(money: Int, coins: List[Int]): Int = {
  def countChangeInt(money: Int, coins: List[Int]): Int ={
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChangeInt(money - coins.head, coins) + countChangeInt(money, coins.tail)

  }

  countChangeInt(money, coins)

}

countChange(4,List(1,2))





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

  balanceInternal(0,0,chars)
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
balance(":-)".toList)
balance("())(".toList)




def pascal(c: Int, r: Int): Int = {

  def pasInternal(c: Int, r: Int) :Int = {
    if (root(c, r) || boundary(c,r)) {
      1
    } else if(invalid(c,r)){
      0
    } else{
      pasInternal(c-1, r-1)  + pasInternal(c, r-1)
    }
  }


  def root(c: Int, r: Int) = {
    if ((c == 0) && (r == 0)) true else false
  }

  def boundary(c: Int, r: Int) = {
    if ((c == 0) || (c == r)) true else false
  }
  def invalid(c: Int, r: Int) = {
    if ((c<0 || r<0) || (c >r)) true else false
  }

  pasInternal(c,r)
}

pascal(0,0)
pascal(0,1)
pascal(3,3)
pascal(2,4)
pascal(3,4)
pascal(2,5)
pascal(1,5)
