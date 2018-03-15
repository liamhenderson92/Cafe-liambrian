case class Water(temperature: Double = 0)

case class FrothingException(msg: String) extends Exception(msg)
case class BrewingException(msg: String) extends Exception(msg)
case class GrindingException(msg: String) extends Exception(msg)

trait Milk
trait FrothedMilk extends Milk
final case class FrothedWholeMilk() extends FrothedMilk

final case class WholeMilk() extends Milk
final case class SemiSkimmedMilk() extends Milk

trait Beans
trait CoffeeBeans extends Beans
final case class ArabicaBeans() extends CoffeeBeans
final case class GroundCoffee()


case class Coffee(water: Water, groundCoffee: GroundCoffee, milk: Option[FrothedMilk] = None) {
  def addMilk(frothedMilk: FrothedMilk): Coffee = this.copy(Water(water.temperature - 5), groundCoffee, Some(FrothedWholeMilk()))
}

object Cafe extends App {

  def heat(water: Water, temperature: Double = 40D): Water = {
    water.copy(temperature)
  }

  def grind(beans: Beans): GroundCoffee = {
    beans match {
      case ArabicaBeans() => GroundCoffee()
      case _ => throw GrindingException("Incorrect Beans")
    }
  }

  def frothMilk(milk: Milk): FrothedMilk = {
    milk match {
      case WholeMilk() => FrothedWholeMilk()
      case _ => throw FrothingException("You need to use Whole Milk")
    }
  }

  def brew(water: Water, coffee: GroundCoffee, milk: Option[FrothedMilk] = None): Coffee = {
    (water, milk) match {
      case (w, Some(FrothedWholeMilk())) if w.temperature >= 40 =>
        println(s"You	have brewed	the	following coffee: Coffee at ${water.temperature - 5} degrees with Whole Milk")
        Coffee(water, coffee).addMilk(FrothedWholeMilk())
      case (w, _) if w.temperature >= 40 =>
        println(s"You	have brewed	the	following coffee: Coffee at ${water.temperature} without milk")
        Coffee(water, coffee)
      case (_, _) => throw BrewingException("The water is too cold")
    }
  }

  val groundCoffee = grind(ArabicaBeans())
  val heatedWater = heat(Water(20))
  val frothyMilk = frothMilk(WholeMilk())

  brew(heatedWater, groundCoffee, Some(frothyMilk))
  brew(heatedWater, groundCoffee)
}
