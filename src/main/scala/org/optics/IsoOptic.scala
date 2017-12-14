package org.optics

import monocle.Iso

/**
  * An Iso is an optic which converts elements of type S into elements of type A without loss.
  */
object IsoOptic {

  val streetToTuple = Iso[Street,(Int,String)](s => (s.number, s.name)){case (number, name) => Street(number,name)}

  def getStreetToTupleIso(street: Street):(Int,String) = {
    streetToTuple.get(street)
  }

  def getTupleToStreetIso(tuple: (Int, String)):Street = {
    streetToTuple.reverseGet(tuple)
  }



  val employeeToTuple = Iso[Employee,(Int,String)](e =>(e.company.address.street.number,e.company.address.street.name)){
    case
      (number, name) => Employee("empresa",Company("company",Address("direccion",Street(number,name))))
  }
  val composeIso = LensOptic.employeeLens composeLens LensOptic.companyLens composeLens LensOptic.addressLens composeIso streetToTuple


  def getEmployeeToTuple(employee: Employee):(Int,String) = {
    composeIso.get(employee)
  }
  def getEmployeeToTuple2(employee: Employee):(Int,String) = {
    employeeToTuple.get(employee)
  }
  def getTupleToEmployee(tuple: (Int, String)):Employee = {
    employeeToTuple.reverseGet(tuple)
  }
}