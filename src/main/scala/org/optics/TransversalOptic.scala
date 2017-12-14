package org.optics

import monocle.Traversal
import scalaz.std.list._

/**
  * A Traversal is the generalisation of an Optional to several targets. In other word, a Traversal allows to focus from a
  * type S into 0 to n values of type A.
  */

object TransversalOptic {
  val eachListOptics: Traversal[List[Int],Int] = Traversal.fromTraverse[List, Int]

  def setAllList(xs: List[Int], e: Int): List[Int] = {
    eachListOptics.set(e)(xs)
  }

  def getAllList(xs: List[Int]): List[Int] = {
    eachListOptics.getAll(xs)
  }

  def getHeadOption(xs: List[Int]): Option[Int] = {
    eachListOptics.headOption(xs)
  }

  def increaseUnitAllList(xs: List[Int]): List[Int] = {
    eachListOptics.modify(_ + 1)(xs)
  }

  def findElementGreater3List(xs: List[Int]): Option[Int] = {
    eachListOptics.find(_ > 3)(xs)
  }

  def allElementGreater3List(xs: List[Int]): Boolean = {
    eachListOptics.all(_ > 3)(xs)
  }

}
