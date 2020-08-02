object DifferenceOfSquares {

  def sumOfSquares(n: Int): Int = 
    List.range(1, n + 1).reduce((acc, value) => acc + value*value)

  def squareOfSum(n: Int): Int = n*n*(n + 1)*(n + 1)/4

  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)
}
