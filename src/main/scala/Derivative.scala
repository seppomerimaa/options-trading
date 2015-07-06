import scala.math._
/**
 * Created by McFly on 7/5/15.
 */
trait Derivative {
  def strike: Long
  def last: Long
  def profitAt(s: Long): Long
}

case class CallOption(k: Long, l: Long, premium: Long) extends Derivative {
  override def strike = k
  override def last = l
  override def profitAt(s: Long) = max(0L, k - s) - premium
}

case class PutOption(k: Long, l: Long, premium: Long) extends Derivative {
  override def strike = k
  override def last = l
  override def profitAt(s: Long) = max(0L, s - k) - premium
}