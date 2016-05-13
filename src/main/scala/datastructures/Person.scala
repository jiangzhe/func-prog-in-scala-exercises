package datastructures

/**
  * Person
  * --
  * Desc:
  * Changes:
  * 2016/4/28 - Created by z_jiang.
  * --
  */
case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))
}