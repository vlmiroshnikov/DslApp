package dsl.filter

trait Filter {
  def accept(item: Item): Boolean
}