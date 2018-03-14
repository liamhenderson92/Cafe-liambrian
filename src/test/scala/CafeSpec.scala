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
      Cafe.grind("Arabica Beans") mustEqual "GroundCoffee"
    }

    "return 'Incorrect Beans' when given 'Baked Beans'" in {
      Cafe.grind("Baked Beans") mustEqual "Incorrect Beans"
    }

    "return 'FrothedMilk' when given 'WholeMilk" in {
      Cafe.frothMilk("WholeMilk") mustEqual "FrothedMilk"
    }

    "throw new IllegalArgumentException when given SemiSkimmedMilk" in {
      val e = intercept[FrothingException] {
        Cafe.frothMilk("SemiSkimmedMilk") mustEqual "You need to use Whole Milk"
      }
    }
  }
}
