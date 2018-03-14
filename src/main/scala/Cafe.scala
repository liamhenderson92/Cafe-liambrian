case class Water(temperature : Double = 0)

object Cafe extends App {

  type CoffeeBeans = String
  type GroundCoffee = String
  type ArabicaBeans = String

  def heat(water: Water, temperature: Double = 40D): Water = {
    water.copy(40D)

  }

  def grind(beans: CoffeeBeans) : GroundCoffee = {
    s"Ground Coffee of $beans"
  }

}
