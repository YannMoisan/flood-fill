import scala.util.Try
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}

case class Person(name: String, age: Int, weight: Int)

object Person {

  /** create a person from a String, receives from an external system, containing name, age and weight separated by a space */
  def parse(s: String): Person = {
    val name = s.split(" ")(0)
    val age = s.split(" ")(1).toInt
    val weight = s.split(" ")(2).toInt
    Person(name, age, weight)
  }

  def parse2(s: String) : Option[Person] = {
    s.split(" ") match {
      case Array(name, age, weight) =>
        val age_ = Try(age.toInt).toOption
        val weight_ = Try(weight.toInt).toOption
        (age_, weight_) match {
          case (Some(a), Some(w)) => Some(Person(name, a, w))
          case _ => None
        }
      case _ => None
    }
  }

  def parse3(s: String) : Option[Person] = {
    s.split(" ") match {
      case Array(name, age, weight) =>
        val age_ = Try(age.toInt).toOption
        val weight_ = Try(weight.toInt).toOption

        (Option(name) |@| age_ |@| weight_).map(Person.apply)
      case _ => None
    }
  }

  def parse4(s: String) = {
    def parse(s: String) : Validated[String, Int] = {
      Try(s.toInt).toOption match {
        case Some(i) => Valid(i)
        case None => Invalid("wrong")
      }
    }

    val splitted = s.split(" ")
    if (splitted.size != 3) {
      None
    } else {
      val Array(name, age, weight) = splitted
      val age_ = parse(age).toValidatedNel
      val weight_ = parse(weight).toValidatedNel

      val name_ : ValidatedNel[String, String] = Valid(name).toValidatedNel

      (name_ |@| age_ |@| weight_).map(Person.apply)

    }
  }


}
