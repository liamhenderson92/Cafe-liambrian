import org.scalatest.{AsyncWordSpec, MustMatchers}

class CafeSpec extends AsyncWordSpec with MustMatchers {

  implicit def ec = Cafe.ec

  "Cafe" must {

    "return 'water with a temperature of 40' when given 'water with a temperature of 20'" in {
      Cafe.heat(Water(20)) map
        { h => h mustEqual Water(40D)
      }
    }

    "return 'water with a temperature of 40' when given water with no temperature" in {
      Cafe.heat(Water()) map { h =>
        h mustEqual Water(40D)
      }
    }

    "return 'GroundCoffee' when given 'Arabica Beans'" in {
      Cafe.grind(ArabicaBeans()) map { h =>
        h mustBe GroundCoffee(ArabicaBeans())
      }
    }

    "return 'Incorrect Beans' when given 'Baked Beans'" in {
      final case class BakedBeans() extends Beans
      recoverToSucceededIf[GrindingException] {
        Cafe.grind(BakedBeans())
      }
    }

    "return 'FrothedMilk' when given 'WholeMilk" in {
      Cafe.frothMilk(WholeMilk()) map { h =>
        h mustBe FrothedWholeMilk()
      }
    }

    "throw new 'FrothingException' when given 'SemiSkimmedMilk'" in {
      recoverToSucceededIf[FrothingException] {
        Cafe.frothMilk(SemiSkimmedMilk())
      }
    }

    "throw new 'BrewingException' when given water with a temperature less than 40 in brew function" in {
      recoverToSucceededIf[BrewingException] {
        Cafe.brew(Water(5), GroundCoffee(ArabicaBeans()))
      }
    }

    "return Coffee when given water 40 degrees or more and Ground Coffee in brew function" in {
      Cafe.brew(Water(40), GroundCoffee(ArabicaBeans())) map { h =>
        h mustEqual Coffee(Water(40), GroundCoffee(ArabicaBeans()))
      }
    }

    "return White Coffee at 35 degrees when given water of 40 degrees and Ground Coffee in brew function" in {
      Cafe.brew(Water(40), GroundCoffee(ArabicaBeans()), Some(FrothedWholeMilk())) map { h =>
        h mustEqual Coffee(Water(35), GroundCoffee(ArabicaBeans()), Some(FrothedWholeMilk()))
      }
    }

    "return Coffee when given water 40 degrees or more and Ground Coffee with no milk in prepare coffee function" in {
      Cafe.prepareCoffee(ArabicaBeans(),Water(40), None) map { h =>
        h mustEqual Coffee(Water(40), GroundCoffee(ArabicaBeans()))
      }
    }

    "return Coffee when given water 35 degrees or more and Ground Coffee with milk in prepare coffee function" in {
      Cafe.prepareCoffee(ArabicaBeans(),Water(40), Some(WholeMilk())) map { h =>
        h mustEqual Coffee(Water(35), GroundCoffee(ArabicaBeans()), Some(FrothedWholeMilk()))
      }
    }

    "throw new Grinding Exception when given 'Robusta Beans" in {
      recoverToSucceededIf[GrindingException] {
      Cafe.prepareCoffee(RobustaBeans(),Water(40), Some(WholeMilk()))
      }
    }
  }
}
