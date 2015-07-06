/**
 * Various Strategy classes, etc.
 */
trait Strategy {
  def maxProfit: Long
  def maxLoss: Long
  def sharpe: Double = maxProfit.toDouble / maxLoss
}

abstract class StrategyBase(p1: Long, p2: Long) extends Strategy {
  val buyStrategies: List[Derivative]
  val sellStrategies: List[Derivative]

  private val prices = p1 to p2 by Utils.MICROS_IN_CENT
  private val profitsFromBuys = prices.map(price => buyStrategies.map(strat => strat.profitAt(price)).sum)
  private val profitsFromSells = prices.map(price => sellStrategies.map(strat => strat.profitAt(price)).sum)
  private val profits = profitsFromBuys.zip(profitsFromSells).map( { case (buyProfit, sellProfit) => buyProfit - sellProfit } )

  def maxProfit: Long = profits.max
  def maxLoss: Long = profits.min
}

class NakedCall(strike: Long, premium: Long, numCalls: Int, p1: Long, p2: Long) extends StrategyBase(p1, p2) {
  private val call = new CallOption(strike, 0L, premium)
  override val buyStrategies = List(call)
  override val sellStrategies = List()
}

class NakedPut(strike: Long, premium: Long, numCalls: Int, p1: Long, p2: Long) extends StrategyBase(p1, p2) {
  private val put = new PutOption(strike, 0L, premium)
  override val buyStrategies = List(put)
  override val sellStrategies = List()
}

class CallSpread(
                  buyStrike: Long,
                  buyPremium: Long,
                  sellStrike: Long,
                  sellPremium: Long,
                  numCalls: Int,
                  p1: Long,
                  p2: Long
                  ) extends StrategyBase(p1, p2) {
  override val buyStrategies = List(new CallOption(buyStrike, 0L, buyPremium))
  override val sellStrategies = List(new CallOption(sellStrike, 0L, sellPremium))
}

class PutSpread(
                 buyStrike: Long,
                 buyPremium: Long,
                 sellStrike: Long,
                 sellPremium: Long,
                 numCalls: Int,
                 p1: Long,
                 p2: Long
                 ) extends StrategyBase(p1, p2) {
  override val buyStrategies = List(new PutOption(buyStrike, 0L, buyPremium))
  override val sellStrategies = List(new PutOption(sellStrike, 0L, sellPremium))
}


