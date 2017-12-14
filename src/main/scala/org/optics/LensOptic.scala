package org.optics

import monocle.Lens
import monocle.macros.GenLens


object LensOptic{

  val streetLens: Lens[Street,String] = GenLens[Street](_.name)
  val addressLens: Lens[Address,Street] = GenLens[Address](_.street)
  val companyLens: Lens[Company,Address] = GenLens[Company](_.address)
  val employeeLens: Lens[Employee,Company] = GenLens[Employee](_.company)

  val composeLens: Lens[Employee, String] = employeeLens composeLens companyLens composeLens addressLens composeLens streetLens
  val modifyOpticsLensStreetName: Employee => Employee = composeLens.modify(_.capitalize)
  // o tambien: composeLens.modify(n => n.capitalize)


  def capitalizeStreetName(employee: Employee):Employee = {
    employee.copy(
      company = employee.company.copy(
        address = employee.company.address.copy(
          street = employee.company.address.street.copy(
            name = employee.company.address.street.name.capitalize
          )
        )
      )
    )
  }

  def getStreetName(street: Street):String = {
    streetLens.get(street)
  }

  def getStreetName(employee: Employee):String = {
    composeLens.get(employee)
  }

  def getStreetNumber(employee: Employee):Int = {
    import monocle.macros.syntax.lens._

    employee.lens(_.company.address.street.number).get
  }

  def capitalizeStreetNameOpticsLens(employee: Employee):Employee = {
    modifyOpticsLensStreetName(employee)
  }


  import scalaz.std.list._
  def neighbors(n: Int): List[Int] = if(n > 0) List(n - 1, n + 1) else List(n + 1)

  val streetNumberLens: Lens[Street,Int] = GenLens[Street](_.number)
  val composeNumberLens: Lens[Address, Int] = addressLens composeLens streetNumberLens

  def increaseNumberStreetOpticsLens(address: Address):Address = {
    import monocle.macros.syntax.lens._
    address.lens(_.street.number).modify(_ + 1)
  }

  def increaseNumberStreetOpticsLens2(address: Address):Address = {
    composeNumberLens.modify(_ + 1)(address)
  }

  def neighborsStreetOpticsLens(address: Address):List[Address] = {
    composeNumberLens.modifyF[List](neighbors)(address)
  }
}