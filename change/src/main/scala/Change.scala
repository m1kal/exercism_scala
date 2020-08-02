object Change {
  def simpleFind(amount: Int, coins: List[Int], soFar: List[Int]=List()) :Option[List[Int]] = {
    if (amount < 0)
      None
    else if (amount == 0)
      Some(soFar)
    else {
      val maxPossibleCoin = coins.sorted.reverse.find(_ <= amount)
      maxPossibleCoin match {
        case Some(maxCoin) => simpleFind(amount - maxCoin, coins, maxCoin::soFar)
        case None => None
      }
    }
  }

  def findCoins(amount: Int, coins: List[Int]) :List[List[Int]] = {
    if (amount < 0 || coins.isEmpty)
      List[List[Int]]()
    else if (amount == 0)
      List(List[Int]())
    else {
      val coin :: rest = coins
      findCoins(amount - coin, coins).map(coin :: _) ::: findCoins(amount,rest)
    }
  }
  def canBeSimplified(coins:List[Int]) = {
    coins.isEmpty ||
      coins.sliding(2, 1).filter{
        case first::second::Nil => 2*first > second
      }.isEmpty
  }
  def findFewestCoinsGeneral(amount:Int, coins:List[Int]) = {
    val partitions = findCoins(amount, coins)
    if (partitions.isEmpty)
      None
    else
      Some(partitions.sortBy(_.length).head)
  }

  def findFewestCoins(amount: Int, coins: List[Int]) :Option[List[Int]] = {
    if (canBeSimplified(coins))
      simpleFind(amount, coins) match {
        case a:Some[List[Int]] => a
        case None => findFewestCoinsGeneral(amount, coins)
     }
    else 
      findFewestCoinsGeneral(amount, coins)
  }
}
