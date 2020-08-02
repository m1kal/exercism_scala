case class ComplexNumber(real: Double = 0, imaginary: Double = 0) {
  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)
  def -(other: ComplexNumber) =
    ComplexNumber(real - other.real, imaginary - other.imaginary)
  def *(other: ComplexNumber) =
    ComplexNumber(real * other.real - imaginary * other.imaginary,
                  real * other.imaginary + imaginary * other.real)
  def /(other: ComplexNumber) =
    this * other.conjugate * ComplexNumber(1 / other.abs / other.abs)
  def conjugate = ComplexNumber(real, -imaginary)
  def abs = math.sqrt(real * real + imaginary * imaginary)
}

object ComplexNumber {
  def exp(value: ComplexNumber) = value match {
    case ComplexNumber(abs, phase) =>
      this(math.cos(phase), math.sin(phase)) * this(math.exp(abs))
  }
}
