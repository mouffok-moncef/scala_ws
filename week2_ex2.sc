def abs (a:Double) :Double =  if (a<0) -a else a

def findFixPoint (f:Double=>Double) (a:Double) : Double = {
  def iterate(guess:Double):Double = {
    println(s"guess = ${guess}")
    val next = f(guess)
    if (abs(next - guess) / guess < 0.0001) next
    else iterate(next)
  }
  iterate(a)
}

def averageDamp(f:Double=>Double)(x:Double):Double = (x+f(x))/2

def sqrt(x:Double): Double = findFixPoint(averageDamp(y=>x/y)) (1)

sqrt(2)