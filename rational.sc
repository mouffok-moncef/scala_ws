class Rational(x : Int, y:Int){
  require(y!=0, "denominator must not be 0")

  def this (x:Int)= this(x,1)

  private def gcd(a:Int, b:Int) : Int = if (b==0) a else gcd(b, a%b)
  //private val g = gcd(x,y)
  def numer=x
  def denom=y
  def < (that:Rational) : Boolean=
    numer * that.denom < that.numer * denom
  def max(that:Rational) : Rational = if (this < that) that else this
  def + (that : Rational) : Rational =
    new Rational(numer*that.denom + that.numer*denom ,
      denom * that.denom)
  def unary_- : Rational = new Rational(-numer, denom)
  def - (that : Rational) : Rational = this + -that
  override def toString = numer/gcd(numer,denom)+"/"+denom/gcd(numer,denom)
}
val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)
x.numer
x.denom
x + y
-x
x - y
x - y - z
y + y
z < y
z max y
//val strange= new Rational(1,0)
//strange.add(strange)

val t = new Rational(4)