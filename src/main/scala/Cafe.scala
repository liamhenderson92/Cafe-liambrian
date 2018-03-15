
case class Water(temperature : Double = 0)
case class FrothingException(msg: String) extends Exception
case class BrewingException(msg: String) extends Exception

object Cafe extends App {

  type CoffeeBeans = String
  type GroundCoffee = String
  type Milk = String
  type FrothedMilk = String
  case class Coffee(water: Water, groundCoffee: GroundCoffee, milk: Option[FrothedMilk] = None) {
    def addMilk(FrothedMilk : String) : Coffee = this.copy(water,groundCoffee,Some("FrothedMilk"))
  }

  def heat(water: Water, temperature: Double = 40D): Water = {
    water.copy(temperature)

  }

  def grind(beans: CoffeeBeans): GroundCoffee = {
    beans match {
      case "Arabica Beans" => "GroundCoffee"
      case _ => "Incorrect Beans"
    }
  }

  def frothMilk(milk: Milk): FrothedMilk = {
    milk match {
      case "WholeMilk" => "FrothedMilk"
      case _ => throw FrothingException("You need to use Whole Milk")
    }
  }

  def brew(water: Water, coffee: GroundCoffee, milk: Option[FrothedMilk] = None): Coffee = {
    water match {
      case x if x.temperature >= 40  && milk.isEmpty =>
        println(s"You	have brewed	the	following coffee: Coffee at ${water.temperature} without milk")
        Coffee(water,coffee)
      case y if y.temperature >= 40  && milk.contains("FrothedMilk") =>
        println(s"You	have brewed	the	following coffee: Coffee at ${water.temperature-5} degrees with Whole Milk")

        Coffee(Water(water.temperature - 5),coffee,Some("FrothedMilk"))
      case _ => throw BrewingException("The water is too cold")
    }
  }


    val groundCoffee = grind("Arabica Beans")
    val heatedWater = heat(Water(40))
    val frothyMilk = frothMilk("WholeMilk")

  brew(heatedWater, groundCoffee, Some(frothyMilk))
  brew(heatedWater, groundCoffee)
}
