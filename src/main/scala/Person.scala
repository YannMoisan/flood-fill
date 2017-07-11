case class Person(name: String, age: Int, weight: Int)

object Person {

  /** create a person from a String, receives from an external system, containing name, age and weight separated by a space */
  def parse(s: String): Person = {
    val name = s.split(" ")(0)
    val age = s.split(" ")(1).toInt
    val weight = s.split(" ")(2).toInt
    Person(name, age, weight)
  }
}
