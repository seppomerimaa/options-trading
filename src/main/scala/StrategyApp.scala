import scala.io.StdIn
/**
 * Created by McFly on 7/8/15.
 */
object StrategyApp extends App{
  val stocksAndChains = Map("NBG" -> "NBG.txt", "GREK" -> "GREK.txt")

  var stock = "" // I ain't sorry
  while (!stocksAndChains.contains(stock)) {
    stock = StdIn.readLine("What stock are you interested in? ")
  }

  var budget = -1.0
  while (budget < 0) {
    print("What's you budget in dollars? ")
    try {
      budget = StdIn.readDouble()
    } catch {
      case e:NumberFormatException => println("Just numbers, no dollar signs or words, please.")
    }
  }

  val recommender = new StrategyRecommender(stocksAndChains(stock), budget)

  while (true) {
    val line = StdIn.readLine("Now give me p1 and p2 separated by a space. ")
    try {
      val p1 = line.split(" ")(0).toDouble
      val p2 = line.split(" ")(1).toDouble
      if (p1 >= 0 && p2 >= 0) {
        println(s"Stock: $stock Budget: $budget p1: $p1 p2: $p2")

        val recommendations = recommender.getTopTen(p1, p2)
        recommendations.foreach(println)
      } else {
        println("p1 and p2 have to be >= 0.")
      }

    } catch {
      case e: NumberFormatException => println("Just two numbers, no dollar signs or words, please.")
      case e: ArrayIndexOutOfBoundsException => println("Ya gotta give me two numbers.")
    }
  }
}
