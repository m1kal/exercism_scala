object SpaceAge {
  val Planets = Array(0.2408476, 0.61519726, 1.0, 1.8808158,
                      11.862615, 29.4474988, 84.016846, 164.79132)

  val onMercury =  onEarth(_ : Double) / Planets(0)
  val onVenus =  onEarth(_ : Double) / Planets(1)
  val onEarth = (_ : Double) / 60 / 60 / 24 / 365.25
  val onMars =  onEarth(_ : Double) / Planets(3)
  val onJupiter =  onEarth(_ : Double) / Planets(4)
  val onSaturn =  onEarth(_ : Double) / Planets(5)
  val onUranus =  onEarth(_ : Double) / Planets(6)
  val onNeptune =  onEarth(_ : Double) / Planets(7)
}

