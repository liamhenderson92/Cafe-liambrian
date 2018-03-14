case class Water(temperature : Double = 0)

case class FrothingException(msg: String) extends Exception

object Cafe extends App {

  type CoffeeBeans = String
  type GroundCoffee = String
  type Milk = String
  type FrothedMilk = String

  def heat(water: Water, temperature: Double = 40D): Water = {
    water.copy(40D)

  }

  def grind(beans: CoffeeBeans) : GroundCoffee = {
    beans match {
      case "Arabica Beans" => "GroundCoffee"
      case _ => "Incorrect Beans"
    }
  }

  def frothMilk(milk: Milk): FrothedMilk = {
    milk match {
      case "WholeMilk" => "FrothedMilk"
      case _ => throw new FrothingException("You need to use Whole Milk")
    }
  }

}
