package dsl

import filter.Item

import scala.collection.immutable.HashMap

case class Person( country :String, age :Int) extends Item {
  val map = HashMap("country" -> country, "age" -> age )
  override def prop(key: String): Option[Any] = {
    map.get(key)
  }
}
