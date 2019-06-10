object BookStore {
  def discount(amount: Int) = {
    if (amount > 3) 1.0-amount*0.05 else 1.05 - amount * 0.05
  }

  def canExtractTwoFours(order : Seq[Int]) = {
    (order.size == 5) && (order(2) == order(4)) && (order(1) < order(2))
  }

  def makeMaxSets(order : Tuple2[List[Int],Seq[Int]]) : List[Int] = { 
    if (order._2.size == 0)
      order._1
    else
      if (canExtractTwoFours(order._2))
        makeMaxSets((List(4,4) ::: order._1,
                   order._2.zip(List(1,1,2,2,2)).map(x=>x._1-x._2).filter(_>0)))
      else
        makeMaxSets((order._2.size :: order._1, order._2.map(_-1).filter(_>0)))
  }

  def total(order : List[Int]) = {
    val repetitions = (1 to 5).map(item=>order.count(_==item)).filter(_>0)
    (makeMaxSets((List(),repetitions.sorted)).
      map((x:Int)=>discount(x)*x).foldLeft(0.0)(_+_)*800).round
  }
}

