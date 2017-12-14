package org.optics

case class Street(number: Int, name: String)
case class Address(city: String, street: Street)
case class Company(name: String, address: Address)
case class Employee(name: String, company: Company)

sealed trait Data
case object Null extends Data
case class Str(v: String) extends Data
case class Num(v: Double) extends Data
case class Obj(v: Map[String, Data]) extends Data