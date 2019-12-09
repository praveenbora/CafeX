package com.capgemini.cafex
import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode.HALF_UP

//CafeX (menu,purchased Item Names)
case class CafeX(menu: Menu, itemNames: String*) {
  
  //subTotal returns total amount of all purchased items 	
  val subTotal: BigDecimal = itemNames.flatMap(menu.priceOf).sum
  
  //serviceCharge of all purchased items
  val serviceCharge: BigDecimal = calculateServiceCharge(analyzeOrders())

  //Total bill after applying service charge on subTotal of all purchased items
  val total = subTotal+serviceCharge

  // BillAnalysisResult contains drinksOnly and haveHotFood
  case class BillAnalysisResult(drinksOnly: Boolean, haveHotFood: Boolean)

  /**
   * this function is to analyze orders
   * @return BillAnalysisResult
   */
  protected def analyzeOrders(): BillAnalysisResult = {
    itemNames
      .flatMap(menu.findByName)
      .foldLeft(BillAnalysisResult(drinksOnly = true, haveHotFood = false)) {
        (result, menuItem) => BillAnalysisResult(
          result.drinksOnly & menuItem.isDrink,
          result.haveHotFood | (menuItem.category == "Hot" && !menuItem.isDrink)
        )
      }
  }

  /**
   * this function is to calculate ServiceCharge
   * @param billAnalysisResult
   * @return service charge
   */
  protected def calculateServiceCharge(billAnalysisResult: BillAnalysisResult): BigDecimal = {
    if(billAnalysisResult.drinksOnly) { 0 }
    else if(!billAnalysisResult.haveHotFood) { (subTotal * 0.1).setScale(2,HALF_UP) }
    else { (subTotal * 0.2).setScale(2,HALF_UP) min 20 }
  }
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