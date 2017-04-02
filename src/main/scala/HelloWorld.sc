"Hello World"

def abs(x: Double) = if (x < 0) -x else x


def sqrt(x: Double) = {

  def sqrIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2


  sqrIter(1.0)
}

sqrt(2)
sqrt(4)

// Euclid algo
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)

factorial(4)
