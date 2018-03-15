import Cafe._
import org.scalatest.{MustMatchers, WordSpec}

class CafeSpec extends WordSpec with MustMatchers {

  "Cafe" must {

    "return 'water with a temperature of 40' when given 'water with a temperature of 20'" in {
      Cafe.heat(Water(20)) mustEqual Water(40D)
    }

    "return 'water with a temperature of 40' when given water with no temperature" in {
      Cafe.heat(Water()) mustEqual Water(40D)
    }

    "return 'GroundCoffee' when given 'Arabica Beans'" in {
      Cafe.grind(ArabicaBeans()) mustBe GroundCoffee()
    }

    "return 'Incorrect Beans' when given 'Baked Beans'" in {

      final case class BakedBeans() extends Beans
      intercept[GrindingException] {
        Cafe.grind(BakedBeans()) mustEqual "Incorrect Beans"
      }
    }

    "return 'FrothedMilk' when given 'WholeMilk" in {
      Cafe.frothMilk(WholeMilk()) mustBe FrothedWholeMilk()
    }

    "throw new 'FrothingException' when given 'SemiSkimmedMilk'" in {
      intercept[FrothingException] {
        Cafe.frothMilk(SemiSkimmedMilk()) mustEqual "You need to use Whole Milk"
      }
    }

    "throw new 'BrewingException' when given water with a temperature less than 40" in {
      intercept[BrewingException] {
        Cafe.brew(Water(5), GroundCoffee()) mustEqual "The water is too cold"
      }
    }

    "return Coffee when given water 40 degrees or more and Ground Coffee" in {
      Cafe.brew(Water(40), GroundCoffee()) mustEqual Coffee(Water(40),GroundCoffee())
    }

    "return White Coffee at 35 degrees when given water of 40 degrees and Ground Coffee" in {
      Cafe.brew(Water(40), GroundCoffee(), Some(FrothedWholeMilk())) mustEqual Coffee(Water(35),GroundCoffee(),Some(FrothedWholeMilk()))
    }
  }
}
