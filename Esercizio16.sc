class Rational(n: Int, d: Int) {
  require(d != 0)
  private val m = mcd(n.abs, d.abs)
  val numer = n / m
  val denom = d / m
  def this(n: Int) = this(n, 1)

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def + (that: Int): Rational =
    new Rational(
      numer + that * denom,
      denom
    )

  def * (that: Rational): Rational =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def * (that: Int): Rational =
    new Rational(
      numer * that,
      denom
    )

  override def toString = numer.toString +"/"+ denom.toString

  private def mcd(a: Int, b: Int): Int =
    if (b == 0) a else mcd(b, a % b)
}

object Rational {
  def apply(n: Int, d: Int) = new Rational(n, d)
  def apply(n: Int) = new Rational(n, 1)
  def unapply(rat: Rational)     =
    Some((rat.numer, rat.denom))
}


val r1 = new Rational(2, 3)
val r2 = r1 + r1
val r3 = r2 + 2
val r4 = r1 * r3
val r5 = r2 * 2
Rational(20, 30)
Rational.unapply(r1)
Rational.apply(10,3)
Rational.apply(10)


val esempio = (par1: Any) =>
  par1 match {
    case (Rational(n, d)) => n / d
    case (n: Int) => n
    case (f: Double) => f.toInt
  }

esempio(13.5634345)
esempio(Rational(4, 2))
esempio(Rational(7))