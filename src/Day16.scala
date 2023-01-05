package dev.agjacome.aoc2022

object Day16 extends Day {

  final case class Valve(id: String, rate: Int, next: Set[String])

  object Valve {

    private val ValveLine =
      """^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)$""".r

    val parse: String => Valve = {
      case ValveLine(id, rate, next) =>
        Valve(id, rate.toInt, next.split(", ").to(Set))
      case line =>
        sys.error(s"Could not parse Valve: ${line}")
    }

  }

  final case class Volcano(valves: List[Valve]) {

    val valveById: String => Option[Valve] =
      valves.map(v => v.id -> v).to(Map).get

    def findPaths(from: String, maxMinutes: Int): Map[Set[String], Int] = {
      @scala.annotation.tailrec
      def loop(minutes: Int, acc: List[(String, Set[String], Int)]): Map[Set[String], Int] =
        if (minutes <= 0) {
          acc.groupMapReduce(_._2)(_._3)(_ max _)
        } else {
          val newStates = acc.flatMap { case (currentId, openIds, pressure) =>
            val currentValve = valveById(currentId).get

            val s0: Set[(String, Set[String], Int)] = {
              currentValve.next.map(next => (next, openIds, pressure))
            }

            val s1: Option[(String, Set[String], Int)] =
              Option.unless(openIds.contains(currentId) || currentValve.rate == 0) {
                (currentId, openIds + currentId, pressure + (minutes - 1) * currentValve.rate)
              }

            s0 ++ s1.to(Set)
          }

          loop(minutes - 1, merge(newStates))
        }

      def merge(acc: List[(String, Set[String], Int)]): List[(String, Set[String], Int)] =
        acc
          .groupMapReduce({ case (currentId, openIds, _) => (currentId, openIds) })(_._3)(_ max _)
          .map { case ((currentId, openIds), pressure) =>
            (currentId, openIds, pressure)
          }
          .to(List)

      loop(maxMinutes, List((from, Set.empty[String], 0)))
    }

  }

  object Volcano {

    def parse(lines: LazyList[String]): Volcano =
      Volcano(lines.map(Valve.parse).to(List))

  }

  def run(lines: LazyList[String]): Result = {
    val volcano = Volcano.parse(lines)

    val part1 = volcano
      .findPaths(from = "AA", maxMinutes = 30)
      .map { case (_, pressure) => pressure }
      .max

    val part2 = {
      val paths = volcano.findPaths(from = "AA", maxMinutes = 26)

      val pressures = paths.to(List).tails.flatMap {
        case Nil => Nil
        case (open1, press1) :: elephants =>
          elephants.collect {
            case (open2, press2) if open1.intersect(open2).isEmpty => press1 + press2
          }
      }

      pressures.max
    }

    Result(part1.toString, part2.toString)
  }

}
