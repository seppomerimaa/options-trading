import scala.math._

trait Derivative {
  val strike: Long
  val premium: Long
  def profitAt(s: Long): Long
}

case class CallOption(strike: Long, premium: Long) extends Derivative {
  override def profitAt(spot: Long) = max(0L, spot - strike) - premium
}

case class PutOption(strike: Long, premium: Long) extends Derivative {
  override def profitAt(spot: Long) = max(0L, strike - spot) - premium
}
