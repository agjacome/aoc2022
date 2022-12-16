package dev.agjacome.aoc2022
package util

sealed trait Tree[A]

object Tree {

  final case class Leaf[A](value: A)                  extends Tree[A]
  final case class Node[A](children: Vector[Tree[A]]) extends Tree[A]

}
