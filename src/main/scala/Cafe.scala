import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

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
final case class RobustaBeans() extends CoffeeBeans

final case class GroundCoffee()

case class Coffee(water: Water, groundCoffee: GroundCoffee, milk: Option[FrothedMilk] = None) {
  def addMilk(frothedMilk: FrothedMilk): Coffee = this.copy(Water(water.temperature - 5), groundCoffee, Some(FrothedWholeMilk()))
}

object Cafe extends App {

  implicit val ec : ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def heat(water: Water, temperature: Double = 40D): Future[Water] = Future {
    Thread.sleep(Random.nextInt(2000))
    println("Water heated to 40 Degrees.")
    water.copy(temperature)
  }

  def grind(beans: Beans): Future[GroundCoffee] = Future {
    Thread.sleep(Random.nextInt(2000))
    beans match {
      case ArabicaBeans() =>
        println("Ground Coffee made from Arabica Beans.")
        GroundCoffee()
      case _ => throw GrindingException("Incorrect Beans")
    }
  }

  def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
    Thread.sleep(Random.nextInt(2000))
    milk match {
      case WholeMilk() =>
        println("Frothed Milk made from WholeMilk.")
        FrothedWholeMilk()
      case _ => throw FrothingException("You need to use Whole Milk")
    }
  }

  def brew(water: Water, coffee: GroundCoffee, milk: Option[FrothedMilk] = None): Future[Coffee] = Future {
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

  def prepareCoffee(beans: Beans, water: Water, milk: Option[Milk]): Future[Coffee] = {

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
    case Success(c) => c
    case Failure(e) => println(e.getMessage)
  }
}