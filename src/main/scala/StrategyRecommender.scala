import scala.io.Source
/**
 * A class that recommends the best from a given set of Strategies for various possible price ranges.
 *
 * NB: all math within this class and the classes that it uses are done 1) in micros rather than dollars and 2) in terms
 * of blocks of 100 options, since that's the minimum block size or some shit like that. Clients of this class don't need
 * to worry about that -- all conversions are handled at the boundaries of this class.
 */
class StrategyRecommender(optionsChain: String, rawBudget: Double) {
  val optionPurchaseBlockSize = 100

  val budget = Utils.dollarsToMicros(rawBudget)

  val lines = Source.fromFile(optionsChain).getLines().map(line => line.split(","))
  val derivatives = lines.map(line => {
    if (line(2) == "call") {
      new CallOption(singleOptionToBlock(line(0).toDouble), singleOptionToBlock(line(1).toDouble))
    } else if (line(2) == "put") {
      new PutOption(singleOptionToBlock(line(0).toDouble), singleOptionToBlock(line(1).toDouble))
    } else {
      throw new IllegalArgumentException("Bad row: " + line)
    }
  }).toList

  def recommendBestStrategy(p1Double: Double, p2Double: Double): Strategy = {
    val p1 = singleOptionToBlock(p1Double)
    val p2 = singleOptionToBlock(p2Double)

    val strategies = Strategy.createAll(p1, p2, budget, derivatives)
    val strategiesAndSharpes = strategies.zip(strategies.map(strategy => strategy.sharpe))
    val bestStrategy = strategiesAndSharpes.maxBy({ case (strategy, sharpe) => sharpe})._1

    bestStrategy
  }

  // Convert prices (premium, strike, whatever) from a single option to the option purchase block size values
  // plus convert from dollar double values to micro long values because fuck floats.
  private def singleOptionToBlock(dollarValue: Double) = Utils.dollarsToMicros(dollarValue * optionPurchaseBlockSize)
}
