import scala.math._
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
  private val call = new CallOption(strike, premium)
  override val buyStrategies = List(call)
  override val sellStrategies = List()
}

class NakedPut(strike: Long, premium: Long, numCalls: Int, p1: Long, p2: Long) extends StrategyBase(p1, p2) {
  private val put = new PutOption(strike, premium)
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
  override val buyStrategies = List(new CallOption(buyStrike, buyPremium))
  override val sellStrategies = List(new CallOption(sellStrike, sellPremium))
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
  override val buyStrategies = List(new PutOption(buyStrike, buyPremium))
  override val sellStrategies = List(new PutOption(sellStrike, sellPremium))
}

object Strategy {
  def createAll(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]) = {
    List[Strategy]()
  }

  private def createNakedCalls(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]) = {
    derivatives.map({
      case derivative: CallOption => new NakedCall(derivative.strike, derivative.premium, (budget / derivative.premium).toInt, p1, p2)
    })
  }

  private def createNakedPuts(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]) = {
    derivatives.map({
      case derivative: PutOption => new NakedPut(derivative.strike, derivative.premium, (budget / derivative.premium).toInt, p1, p2)
    })
  }

  private def createCallSpreads(p1: Long, p2: Long, budget: Long, derivatives: List[Derivative]) = {
    val calls = derivatives.filter({case call: CallOption => true})
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
    val puts = derivatives.filter({case put: PutOption => true})
    val putPairs = puts.flatMap(put1 => puts.map(put2 => (put1, put2))).filter(p => p._1.strike != p._2.strike)
    val spreads = putPairs.map({ case (put1: CallOption, put2: CallOption) =>
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

