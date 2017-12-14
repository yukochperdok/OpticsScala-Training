package org.optics

import monocle.Iso
import org.scalatest.{FlatSpecLike, Matchers}

import scalaz.\/-
import scalaz.-\/

class OpticsTest extends FlatSpecLike
                       with Matchers {

  val street = Street(55, "inventada")
  val employee = Employee("Alfonso", Company("Alfonso Inc.", Address("Pitis",street)))


  "The street" should "change the street name" in {

    val result = LensOptic.capitalizeStreetName(employee)
    result.company.address.street.name shouldBe "Inventada"
  }

  "The street name" should "be returned" in {

    val result = LensOptic.getStreetName(street)
    result shouldBe "inventada"

    val result2 = LensOptic.getStreetName(employee)
    result2 shouldBe "inventada"

    val result3 = LensOptic.getStreetNumber(employee)
    result3 shouldBe 55

  }

  "The street" should "change the street name with optics" in {
    val result = LensOptic.capitalizeStreetNameOpticsLens(employee)
    result.company.address.street.name shouldBe "Inventada"
  }

  "The street" should "transform to tuple and viceversa" in {
    val tupla: (Int, String) = (43,"Casa Pepe")
    val street = IsoOptic.getTupleToStreetIso(tupla)
    street shouldBe Street(43,"Casa Pepe")
    IsoOptic.getStreetToTupleIso(street) shouldBe tupla
    IsoOptic.streetToTuple(tupla) shouldBe street
  }

  "The list of streets" should "transform to vector of streets and viceversa" in {
    def listStreetToVector[Street] = Iso[List[Street], Vector[Street]](_.toVector)(_.toList)

    val street1 = Street(1,"Calle 1")
    val street2 = Street(2,"Calle 2")
    val street3 = Street(3,"Calle 3")

    val vStreet = listStreetToVector.get(List(street1, street2, street3))
    vStreet shouldBe Vector(street1, street2, street3)
    val listStreet = listStreetToVector(Vector(street1, street2, street3))
    listStreet shouldBe List(street1, street2, street3)
  }

  "The employee" should "transform to tuple and viceversa" in {
    val tupla = IsoOptic.getEmployeeToTuple(employee)
    tupla shouldBe IsoOptic.getStreetToTupleIso(street)
    val tupla2 = IsoOptic.getEmployeeToTuple2(employee)
    tupla2 shouldBe IsoOptic.getStreetToTupleIso(street)
    val employee2 = IsoOptic.getTupleToEmployee(tupla2)
    employee2 shouldBe Employee("empresa",Company("company",Address("direccion",IsoOptic.getTupleToStreetIso(tupla2))))
  }

  "The street number" should "increase 1 unit" in {
    LensOptic.increaseNumberStreetOpticsLens(Address("address1",Street(11,"Calle 11"))) shouldBe Address("address1",Street(12,"Calle 11"))
    LensOptic.increaseNumberStreetOpticsLens2(Address("address1",Street(11,"Calle 11"))) shouldBe Address("address1",Street(12,"Calle 11"))
  }

  "Each street number" should "have 2 neighbors" in {
    LensOptic.neighborsStreetOpticsLens(Address("address1",Street(11,"Calle 11"))) shouldBe List(Address("address1",Street(10,"Calle 11")),Address("address1",Street(12,"Calle 11")))
    LensOptic.neighborsStreetOpticsLens(Address("address1",Street(0,"Calle 11"))) shouldBe List(Address("address1",Street(1,"Calle 11")))
  }

  "The string data" should "be returned only for type Str" in {
    PrismOptic.getOptionStr(Str("Hola")).get shouldBe "Hola"
    PrismOptic.getOptionStr(Num(4.5)) shouldBe None
    PrismOptic.getOptionStr(Null) shouldBe None
  }

  "The string data" should "be reversed only for type Str" in {
    PrismOptic.getReverseStr(Str("Hola")) shouldBe Str("aloH")
    PrismOptic.getReverseStr(Num(4.5)) shouldBe Num(4.5)
    PrismOptic.getReverseStr(Null) shouldBe Null
    PrismOptic.getOptionReverseStr(Str("Hola")).get shouldBe Str("aloH")
    PrismOptic.getOptionReverseStr(Num(4.5)) shouldBe None
    PrismOptic.getOptionReverseStr(Null) shouldBe None
  }

  "The number data" should "be returned like a integer only if it's avaliable" in {
    PrismOptic.getOptionInt(Num(4.0)) shouldBe Some(4)
    PrismOptic.getOptionInt(Num(4.5)) shouldBe None
    PrismOptic.getOptionInt(Null) shouldBe None
    PrismOptic.getOptionInt(Str("Hola")) shouldBe None
  }

  "The number data" should "be returned like a double raw only if it's avaliable" in {
    PrismOptic.getRawDouble(Num(4.0)) shouldBe Some(4)
    PrismOptic.getRawDouble(Num(4.5)) shouldBe Some(4.5)
    PrismOptic.getRawDouble(Null) shouldBe None
    PrismOptic.getRawDouble(Str("Hola")) shouldBe None
  }

  "The null data" should "be returned like a unit data" in {
    PrismOptic.getOptionNull(Null).get shouldBe ()
    PrismOptic.getOptionNull(Num(4.5)) shouldBe None
    PrismOptic.getOptionNull(Str("Hola")) shouldBe None
  }

  "The isHeadNonEmpty" should "return true if the list isn't empty" in {
    OptionalOptic.isHeadNonEmpty(List(1,2,3)) shouldBe true
    OptionalOptic.isHeadNonEmpty(List.empty) shouldBe false
  }

  "The getOrModifyHead" should "return head element if the list isn't empty" in {
    OptionalOptic.getOrModifyHead(List(1,2,3)) shouldBe \/-(1)
    OptionalOptic.getOrModifyHead(List.empty) shouldBe -\/(List.empty)
  }

  "The setHead" should "set head element if the list isn't empty" in {
    OptionalOptic.setHead(List(1,2,3),5) shouldBe List(5,2,3)
    OptionalOptic.setHead(List.empty,5) shouldBe List.empty
  }

  "The increaseHead" should "increase one unit the head element if the list isn't empty" in {
    OptionalOptic.increaseHead(List(1,2,3)) shouldBe List(2,2,3)
    OptionalOptic.increaseHead(List.empty) shouldBe List.empty
  }

  "The increaseOptionalHead" should "increase one unit the head element if the list isn't empty" in {
    OptionalOptic.increaseOptionHead(List(1,2,3)) shouldBe Some(List(2,2,3))
    OptionalOptic.increaseOptionHead(List.empty) shouldBe None
  }

}
