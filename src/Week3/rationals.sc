val x = new Rational(1, 2)
x.numer
x.denom
val y = new Rational(2, 3)
val z = new Rational(3, 4)

x.sub(y).sub(z)

x.add(y)


class Rational(x: Int, y: Int){
  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  def numer = x / g
  def denom = y / g

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = numer + "/" + denom
}