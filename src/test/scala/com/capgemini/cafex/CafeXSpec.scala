package com.capgemini.cafex
import org.scalatest._

trait DefaultMenu {
  val defaultMenu: Menu = Menu(
    MenuItem("Cola", isDrink = true, "Cold", 0.5) ::
      MenuItem("Coffee", isDrink = true, "Hot", 1.00) ::
      MenuItem("Cheese Sandwich", isDrink = false,"Cold", 2.00) ::
      MenuItem("Steak Sandwich", isDrink = false, "Hot", 4.50) ::
      Nil)
}

class CafeXSpec extends FlatSpec {

  "Cafe X" should "have a Menu" in new DefaultMenu {
    val cafeX:CafeX= CafeX(defaultMenu)
    assert(!cafeX.menu.items.isEmpty)
  }

  "The Menu" should "have Cola" in new DefaultMenu {
    assert(defaultMenu.findByName("Cola").isDefined)
  }
  it should "have Coffee" in new DefaultMenu {
    assert(defaultMenu.findByName("Coffee").isDefined)
  }
  it should "have Cheese Sandwich" in new DefaultMenu {
    assert(defaultMenu.findByName("Cheese Sandwich").isDefined)
  }
  it should "have Steak Sandwich" in new DefaultMenu {
    val menuItem= MenuItem("Steak Sandwich", isDrink = false, "Hot", 4.50)
    assert(defaultMenu.findByName(menuItem.name).isDefined)
  }

  "Cola" should "cost 50p" in new DefaultMenu {
    private val cola = defaultMenu.findByName("Cola").get
    assert(cola.price == 0.5)
  }
  it should "be categorized as Cold" in new DefaultMenu {
    private val cola = defaultMenu.findByName("Cola").get
    assert(cola.category == "Cold")
  }
  it should "be considered as a Drink" in new DefaultMenu {
    private val cola = defaultMenu.findByName("Cola").get
    assert(cola.isDrink)
  }

  "Coffee" should "cost 1.00 GBP" in new DefaultMenu {
    private val coffee = defaultMenu.findByName("Coffee").get
    assert(coffee.price == 1.00)
  }
  it should "be categorized as Hot" in new DefaultMenu {
    private val coffee = defaultMenu.findByName("Coffee").get
    assert(coffee.category == "Hot")
  }
  it should "be considered as a Drink" in new DefaultMenu {
    private val coffee = defaultMenu.findByName("Coffee").get
    assert(coffee.isDrink)
  }

  "Cheese Sandwich" should "cost 2.00 GBP" in new DefaultMenu {
    private val cheeseSandwich = defaultMenu.findByName("Cheese Sandwich").get
    assert(cheeseSandwich.price == 2.00)
  }
  it should "be categorized as Cold" in new DefaultMenu {
    private val cheeseSandwich = defaultMenu.findByName("Cheese Sandwich").get
    assert(cheeseSandwich.category == "Cold")
  }
  it should "not be considered as a Drink" in new DefaultMenu {
    private val cheeseSandwich = defaultMenu.findByName("Cheese Sandwich").get
    assert(!cheeseSandwich.isDrink)
  }

  "Steak Sandwich" should "cost 4.50 GBP" in new DefaultMenu {
    private val steakSandwich = defaultMenu.findByName("Steak Sandwich").get
    val menuItem= MenuItem("Steak Sandwich", isDrink = false, "Hot", 4.50)
    assert(steakSandwich.price == menuItem.price)
  }
  it should "be categorized as Hot" in new DefaultMenu {
    private val steakSandwich = defaultMenu.findByName("Steak Sandwich").get
    val menuItem= MenuItem("Steak Sandwich", isDrink = false, "Hot", 4.50)
    assert(steakSandwich.category == menuItem.category)
  }
  it should "not be considered as a Drink" in new DefaultMenu {
    private val steakSandwich = defaultMenu.findByName("Steak Sandwich").get
    val menuItem= MenuItem("Steak Sandwich", isDrink = false, "Hot", 4.50)
    assert(steakSandwich.isDrink==menuItem.isDrink)
  }

  // Step1 : Standard Bil
  "Standard Bill - subTotal" should "be able to produce total(3.5) of the passed items(Cola,Coffee,Cheese Sandwich)" in new DefaultMenu {
    private val total = CafeX(defaultMenu, "Cola", "Coffee", "Cheese Sandwich").subTotal
    assert(total == 3.5)
  }
  it should "produce 0 when passed an empty item list" in new DefaultMenu {
    private val total = CafeX(defaultMenu).subTotal
    assert(total == 0)
  }
  it should "ignore item(s) not in the menu when calculating the total" in new DefaultMenu {
    private val total = CafeX(defaultMenu, "No Such Item", "No Such Item 2", "Cola").subTotal
    assert(total == 0.5)
  }

  
}
