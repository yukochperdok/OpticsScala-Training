package org.optics

import monocle.Optional

import scalaz.\/

/**
  * An Optional is an Optic used to zoom inside a Product, e.g. case class, Tuple, HList or even Map. Unlike the Lens, the element that the Optional focuses on may not exist.
  * Optionals have two type parameters generally called S and A: Optional[S, A] where S represents the Product and A an optional element inside of S.
  */
object OptionalOptic {

  // Optica que devuelve el elemento cabecera de una lista de enteros (getOption) o ()
  val headOptional:Optional[List[Int],Int] = Optional[List[Int],Int]{
      case Nil => None
      case x :: xs => Some(x)
    }{ a => {
      case Nil => Nil
      case x :: xs => a :: xs
    }
  }

  def isHeadNonEmpty(xs:List[Int]):Boolean = {
    headOptional.nonEmpty(xs)
  }

  def getOrModifyHead(xs:List[Int]):List[Int]\/Int = {
    headOptional.getOrModify(xs)
  }

  def setHead(xs:List[Int], head: Int):List[Int] = {
    headOptional.set(head)(xs)
  }

  def increaseHead(xs:List[Int]):List[Int] = {
    headOptional.modify(_ + 1)(xs)
  }

  def increaseOptionHead(xs:List[Int]):Option[List[Int]] = {
    headOptional.modifyOption(_ + 1)(xs)
  }



}
