import scala.math._
/**
 * Various Strategy classes, etc.
 */
trait Strategy {
  def maxProfit: Long
  def maxLoss: Long
  def sharpe: Double = maxProfit.toDouble / maxLoss
  val numCalls: Int // should be numOptions but so lazy...
  def info = s"maxProfit: ${maxProfit.toDouble / Utils.MICROS_IN_DOLLAR * numCalls} maxLoss: ${maxLoss.toDouble / Utils.MICROS_IN_DOLLAR * numCalls} sharpe: $sharpe"
}

abstract class StrategyBase(p1: Long, p2: Long) extends Strategy {
  val buyStrategies: List[Derivative]
  val sellStrategies: List[Derivative]

  lazy private val prices = p1 to p2 by Utils.MICROS_IN_CENT
  lazy private val profitsFromBuys = prices.map(price => buyStrategies.map(strat => strat.profitAt(price)).sum)
  lazy private val profitsFromSells = prices.map(price => sellStrategies.map(strat => strat.profitAt(price)).sum)
  lazy private val profits = profitsFromBuys.zip(profitsFromSells).map( { case (buyProfit, sellProfit) => buyProfit - sellProfit } )

  def maxProfit: Long = profits.max
  def maxLoss: Long = abs(profits.min)

  override def toString = s"something else..."
  val scaleDown = Utils.MICROS_IN_DOLLAR
}

class NakedCall(strike: Long, premium: Long, override val numCalls: Int, p1: Long, p2: Long) extends StrategyBase(p1, p2) {
  private val call = new CallOption(strike, premium)
  override val buyStrategies = List(call)
  override val sellStrategies = List()
  override def toString = s"NakedCall -- strike: ${strike / scaleDown.toDouble} premium: ${premium / scaleDown.toDouble} numCalls: $numCalls"
}

class NakedPut(strike: Long, premium: Long, override val numCalls: Int, p1: Long, p2: Long) extends StrategyBase(p1, p2) {
  private val put = new PutOption(strike, premium)
  override val buyStrategies = List(put)
  override val sellStrategies = List()
  override def toString = s"NakedPut -- strike: ${strike / scaleDown.toDouble} premium: ${premium / scaleDown.toDouble} numCalls: $numCalls"
}

class CallSpread(
                  buyStrike: Long,
                  buyPremium: Long,
                  sellStrike: Long,
                  sellPremium: Long,
                  override val numCalls: Int,
                  p1: Long,
                  p2: Long
                  ) extends StrategyBase(p1, p2) {
  override val buyStrategies = List(new CallOption(buyStrike, buyPremium))
  override val sellStrategies = List(new CallOption(sellStrike, sellPremium))
  override def toString = s"Call Spread -- buyStrike: $buyStrike buyPremium: $buyPremium sellStrike: $sellStrike sellPremium: $sellPremium numCalls: $numCalls"
}

class PutSpread(
                 buyStrike: Long,
                 buyPremium: Long,
                 sellStrike: Long,
                 sellPremium: Long,
                 override val numCalls: Int,
                 p1: Long,
                 p2: Long
                 ) extends StrategyBase(p1, p2) {
  override val buyStrategies = List(new PutOption(buyStrike, buyPremium))
  override val sellStrategies = List(new PutOption(sellStrike, sellPremium))
  override def toString = s"Put Spread -- buyStrike: $buyStrike buyPremium: $buyPremium sellStrike: $sellStrike sellPremium: $sellPremium numCalls: $numCalls"

}

object Strategy {
  def createAll(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]): List[Strategy] = {
    createNakedCalls(p1, p2, budget, derivatives) ++ createNakedPuts(p1, p2, budget, derivatives) ++
    createCallSpreads(p1, p2, budget, derivatives) ++ createPutSpreads(p1, p2, budget, derivatives)
  }

  private def createNakedCalls(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]): List[Strategy] = {
    derivatives.filter({ case d: CallOption => true case _ => false}).map(derivative =>
      new NakedCall(derivative.strike, derivative.premium, (budget / derivative.premium).toInt, p1, p2)
    )
  }

  private def createNakedPuts(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]): List[Strategy] = {
    derivatives.filter({case derivative: PutOption => true case _ => false}).map(derivative =>
      new NakedPut(derivative.strike, derivative.premium, (budget / derivative.premium).toInt, p1, p2)
    )
  }

  private def createCallSpreads(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]) = {
    val calls = derivatives.filter({case call: CallOption => true case _ => false})
    val callPairs = calls.flatMap(call1 => calls.map(call2 => (call1, call2))).filter(p => p._1.strike != p._2.strike)

    val spreads = callPairs.map({ case (call1: CallOption, call2: CallOption) =>
        val numCalls1 = max(0, budget / (call1.premium - call2.premium)).toInt
        val numCalls2 = max(0, budget / (call2.premium - call1.premium)).toInt
      List[Strategy](
        new CallSpread(call1.strike, call1.premium, call2.strike, call2.premium, numCalls1, p1, p2),
        new CallSpread(call2.strike, call2.premium, call1.strike, call1.premium, numCalls2, p1, p2)
      )
    })

    spreads.flatten
  }

  private def createPutSpreads(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]) = {
    val puts = derivatives.filter({case put: PutOption => true case _ => false})
    val putPairs = puts.flatMap(put1 => puts.map(put2 => (put1, put2))).filter(p => p._1.strike != p._2.strike)
    val spreads = putPairs.map({ case (put1: PutOption, put2: PutOption) =>
      val numPuts1 = max(0, budget / (put1.premium - put2.premium)).toInt
      val numPuts2 = max(0, budget / (put2.premium - put1.premium)).toInt
      List[Strategy](
        new PutSpread(put1.strike, put1.premium, put2.strike, put2.premium, numPuts1, p1, p2),
        new PutSpread(put2.strike, put2.premium, put1.strike, put1.premium, numPuts2, p1, p2)
      )
    })
    spreads.flatten
  }
}

