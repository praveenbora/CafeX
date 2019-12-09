package com.capgemini.cafex
import scala.math.BigDecimal


//CafeX (menu,purchased Item Names)
case class CafeX(menu: Menu, itemNames: String*) {
  
  //subTotal returns total amount of all purchased items 	
  val subTotal: BigDecimal = itemNames.flatMap(menu.priceOf).sum
  

}

case class Menu(items: List[MenuItem]) {
  private val dictionary: Map[String, MenuItem] =
    items.foldLeft(Map[String, MenuItem]())((d, menuItem) => d + (menuItem.name -> menuItem))

  /**
   * this function is to find the item in the menu
   * @param itemName
   * @return Option[MenuItem]
   */
  def findByName(itemName: String): Option[MenuItem] = dictionary.get(itemName)

  /**
   * this function is to find the price of item in the menu
   * @param itemName
   * @return Option[BigDecimal] - price
   */
  def priceOf(itemName: String):Option[BigDecimal] = dictionary.get(itemName).map(_.price)
}

// menu - itemName,isDrink,category(Hot or Cold),price
case class MenuItem(name: String, isDrink: Boolean, category: String, price: BigDecimal)