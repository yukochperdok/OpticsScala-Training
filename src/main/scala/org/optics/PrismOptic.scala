package org.optics

import monocle.Prism
import monocle.macros.{GenIso, GenPrism}
import monocle.std.double.doubleToInt

/**
  * An Optional is an Optic used to zoom inside a Product, e.g. case class, Tuple, HList or even Map. Unlike the Lens, the element that the
  * Optional focuses on may not exist.
  * Optionals have two type parameters generally called S and A: Optional[S, A] where S represents the Product and A an optional element
  * inside of S.
  */
object PrismOptic {
  //Funcion parcial. Solo aplica a los tipos Str el resto devuelve None
  val stringPrism: Prism[Data, String] = Prism.partial[Data, String]{case Str(v) => v}(Str)
  val numberPrism: Prism[Data, Double] = Prism.partial[Data, Double]{case Num(v) => v}(Num)
  val intPrism: Prism[Data, Int] = numberPrism composePrism doubleToInt

  val numberRawPrism: Prism[Data, Num] = GenPrism[Data, Num]
  val doubleRawPrism: Prism[Data, Double] = numberRawPrism composeIso GenIso[Num,Double]

  val unitPrism: Prism[Data, Unit] = GenPrism[Data, Null.type] composeIso GenIso.unit[Null.type]

  def getOptionStr(data: Data):Option[String] = {
    //Solo devuelve Str
    stringPrism.getOption(data)
  }

  def getReverseStr(data: Data):Data = {
    // Devuelve siempre un Data pero solo hace reverse a los Str
    stringPrism.modify(_.reverse)(data)
  }

  def getOptionReverseStr(data: Data):Option[Data] = {
    // Aplica solo reverse a Str y devuelve un Option. En caso de no ser Str devuelve None
    stringPrism.modifyOption(_.reverse)(data)
  }

  def getOptionInt(data: Data):Option[Int] = {
    // Solo devuelve Int. Si no es Num devuelve None. Si no es parseable a Int devuelve None
    intPrism.getOption(data)
  }

  def getRawDouble(data: Data):Option[Double] = {
    doubleRawPrism.getOption(data)
  }

  def getOptionNull(data: Data):Option[Unit] = {
    unitPrism.getOption(data)
  }

}
