import org.scalatest.{MustMatchers, WordSpec}

class CafeSpec extends WordSpec with MustMatchers {

  "Cafe" must {

    "return 'water with a temperature of 40' when given 'water with a temperature of 20'" in {
      Cafe.heat(Water(20)) mustEqual Water(40D)
    }

    "return 'water with a temperature of 40' when given water with no temperature" in {
      Cafe.heat(Water()) mustEqual Water(40D)
    }

    "return 'Ground Coffee of Arabica beans' when given 'Arabica beans'" in {
      Cafe.grind("Arabica beans") mustEqual "Ground Coffee of Arabica beans"
    }


  }

}
