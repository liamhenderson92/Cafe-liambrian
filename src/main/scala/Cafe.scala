import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

case class Water(temperature: Double = 0)

case class FrothingException(msg: String) extends Exception(msg)
case class BrewingException(msg: String) extends Exception(msg)
case class GrindingException(msg: String) extends Exception(msg)

trait Milk
final case class WholeMilk() extends Milk
final case class SemiSkimmedMilk() extends Milk

trait FrothedMilk extends Milk
final case class FrothedWholeMilk() extends FrothedMilk

trait Beans
trait CoffeeBeans extends Beans
final case class ArabicaBeans() extends CoffeeBeans
final case class RobustaBeans() extends CoffeeBeans

final case class GroundCoffee()

case class Coffee(water: Water, groundCoffee: GroundCoffee, milk: Option[FrothedMilk] = None) {
  def addMilk(frothedMilk: FrothedMilk): Coffee = this.copy(Water(water.temperature - 5), groundCoffee, Some(FrothedWholeMilk()))
}

object Cafe extends App {

  implicit def ec : ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def heat(water: Water, temperature: Double = 40D): Future[Water] = Future {
    println("Heating water...")
    Thread.sleep(Random.nextInt(2000))
    println(Console.GREEN + "Water heated to 40 Degrees." + Console.RESET)
    water.copy(temperature)
  }

  def grind(beans: Beans): Future[GroundCoffee] = Future {
    println("Attempting to grind beans...")
    Thread.sleep(Random.nextInt(2000))
    beans match {
      case ArabicaBeans() =>
        println(Console.GREEN + "Ground Coffee made from Arabica Beans." + Console.RESET)
        GroundCoffee()
      case _ => throw GrindingException("Incorrect Beans")
    }
  }

  def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
    println("Attempting to froth the milk...")
    Thread.sleep(Random.nextInt(2000))
    milk match {
      case WholeMilk() =>
        println(Console.GREEN + "Frothed Milk made from WholeMilk." + Console.RESET)
        FrothedWholeMilk()
      case _ => throw FrothingException("You need to use Whole Milk")
    }
  }

  def brew(water: Water, coffee: GroundCoffee, milk: Option[FrothedMilk] = None): Future[Coffee] = Future {
    println("Brewing coffee...")
    (water, milk) match {
      case (w, Some(FrothedWholeMilk())) if w.temperature >= 40 =>
        println(Console.GREEN + s"You have brewed the following coffee: Coffee at ${water.temperature - 5} degrees with Whole Milk" + Console.RESET)
        Coffee(water, coffee).addMilk(FrothedWholeMilk())
      case (w, _) if w.temperature >= 40 =>
        println(Console.GREEN + s"You have brewed the following coffee: Coffee at ${water.temperature} degrees without milk" + Console.RESET)
        Coffee(water, coffee)
      case (_, _) => throw BrewingException("The water is too cold")
    }
  }

  def prepareCoffee(beans: Beans, water: Water, milk: Option[Milk] = None): Future[Coffee] = {

    print("                        (\n" +
      "                          )     (\n" +
      "                   ___...(-------)-....___\n" +
      "               .-\"\"       )    (          \"\"-.\n" +
      "         .-'``'|-._             )         _.-|\n" +
      "        /  .--.|   `\"\"---...........---\"\"`   |\n" +
      "       /  /    |                             |\n" +
      "       |  |    |                             |\n" +
      "        \\  \\   |            "+Console.RED+"CAFE"+Console.RESET+"             |\n" +
      "         `\\ `\\ |          "+Console.RED+"LiamBrian"+Console.RESET+"          |\n" +
      "           `\\ `|                             |\n" +
      "           _/ /\\                             /\n" +
      "          (__/  \\                           /\n" +
      "       _..---\"\"` \\                         /`\"\"---.._\n" +
      "    .-'           \\                       /          '-.\n" +
      "   :               `-.__             __.-'              :\n" +
      "   :                  ) \"\"---...---\"\" (                 :\n" +
      "    '._               `\"--...___...--\"`              _.'\n" +
      "      \\\"\"--..__                              __..--\"\"/\n" +
      "       '._     \"\"\"----.....______.....----\"\"\"     _.'\n" +
      "          `\"\"--..,,_____            _____,,..--\"\"`\n" +
      "                        `\"\"\"----\"\"\"`\n\n")

    val groundCoffee = grind(beans)
    val heatedWater = heat(water)

    milk match {
      case _ if milk.isDefined =>
        val frothyMilk = frothMilk(milk.get)
        (for {
          ground <- groundCoffee
          water <- heatedWater
          foam <- frothyMilk
        } yield brew(water, ground, Some(foam))).flatten
      case _ =>
        (for {
          ground <- groundCoffee
          water <- heatedWater
        } yield brew(water, ground)).flatten
    }
  }

  val brewedCoffee = prepareCoffee(ArabicaBeans(),Water(2),Some(WholeMilk()))

  brewedCoffee.onComplete {
    case Success(c) => c + sys.exit
    case Failure(e) => println(Console.RED + "Error: " + e.getMessage + Console.RESET) + sys.exit
  }
}