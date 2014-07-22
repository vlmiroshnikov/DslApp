package dsl

import filter.{Filter, Item}

object  CollectionHelper {
  class Holder[T <: Item](source:Seq[T]) {
    def query(f : Filter) : Seq[T] = {
      source.filter(c=> f.accept(c))
    }
  }

  implicit def  wrap[T <: Item](coll : Seq[T]) = new Holder(coll)
}
