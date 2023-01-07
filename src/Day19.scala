package dev.agjacome.aoc2022

import scala.concurrent.duration._

import dev.agjacome.aoc2022.util.ops._

object Day19 extends Day {

  sealed trait Resource

  object Resource {

    case object Ore      extends Resource
    case object Clay     extends Resource
    case object Obsidian extends Resource
    case object Geode    extends Resource

    val all = Set(Ore, Clay, Obsidian, Geode)

  }

  final case class RobotCrew(mine: Map[Resource, Int]) extends AnyVal {

    def add(resource: Resource): RobotCrew =
      RobotCrew(mine.updating(resource, 0)(_ + 1))

    def count(resource: Resource): Int =
      mine.getOrElse(resource, 0)

    def canMine(resources: Iterable[Resource]): Boolean =
      resources.map(count).forall(_ > 0)

  }

  object RobotCrew {

    def of(robots: (Resource, Int)*): RobotCrew =
      RobotCrew(robots.to(Map))

  }

  final case class Inventory(value: Map[Resource, Int]) extends AnyVal {

    def get(resource: Resource): Int =
      value.getOrElse(resource, 0)

    def map(f: (Resource, Int) => Int): Inventory =
      Inventory(value.map { case (k, v) => k -> f(k, v) })

    def increment(amounts: Map[Resource, Int]): Inventory = {
      val updated = amounts.foldLeft(value) { case (acc, (resource, amount)) =>
        acc.updating(resource, 0)(_ + amount)
      }

      Inventory(updated)
    }

    def decrement(amounts: Map[Resource, Int]): Inventory =
      increment(amounts.negated)

  }

  object Inventory {

    val empty: Inventory =
      Inventory(Map.empty)

  }

  final case class Blueprint(id: Int, costs: Resource => Map[Resource, Int]) {

    val highestCost: Resource => Int =
      Resource.all
        .flatMap(costs)
        .groupMapReduce(_._1)(_._2)(_ max _)
        .getOrElse(_, 0)

  }

  object Blueprint {

    private val BlueprintLine =
      """|Blueprint (\d+):
         |\sEach ore robot costs (\d+) ore.
         |\sEach clay robot costs (\d+) ore.
         |\sEach obsidian robot costs (\d+) ore and (\d+) clay.
         |\sEach geode robot costs (\d+) ore and (\d+) obsidian.""".stripMargin.replace("\n", "").r

    val parse: String => Blueprint = {
      case BlueprintLine(id, ore, clay, obs1, obs2, geo1, geo2) =>
        import Resource._

        Blueprint(
          id = id.toInt,
          costs = {
            case Ore      => Map(Ore -> ore.toInt)
            case Clay     => Map(Ore -> clay.toInt)
            case Obsidian => Map(Ore -> obs1.toInt, Clay -> obs2.toInt)
            case Geode    => Map(Ore -> geo1.toInt, Obsidian -> geo2.toInt)
          }
        )

      case line =>
        sys.error(s"Could not parse Blueprint: ${line}")
    }

  }

  final case class Factory(blueprint: Blueprint) {

    import Factory.State

    def run(duration: Duration): Int = {
      var loopCache = Map.empty[State, Int]

      // TODO: make tail recursive or reuse util.Search.DFS
      def loop(state: State): Int =
        if (state.isCompleted) {
          state.geodes
        } else {
          loopCache.get(state).getOrElse {
            val nextGeodes = state.next(blueprint).map(loop)
            val potential  = state.potentialGeodes
            val maxGeodes  = (nextGeodes + potential).max
            loopCache += (state -> maxGeodes)
            maxGeodes
          }
        }

      loop(State.initial(duration))
    }

  }

  object Factory {

    private final case class State(remaining: Duration, inventory: Inventory, robots: RobotCrew) {

      val isCompleted   = remaining <= 0.minutes
      val remainingMins = remaining.toMinutes.toInt

      val geodes          = inventory.get(Resource.Geode)
      val geodeRobots     = robots.count(Resource.Geode)
      val potentialGeodes = geodes + geodeRobots * remainingMins

      def next(blueprint: Blueprint): Set[State] =
        Resource.all
          .filter { resource =>
            def isGeode     = resource == Resource.Geode
            def isBelowMax  = robots.count(resource) < blueprint.highestCost(resource)
            def canBuildNow = robots.canMine(blueprint.costs(resource).keys)

            (isGeode || isBelowMax) && canBuildNow
          }
          .flatMap { robot =>
            val robotCosts = blueprint.costs(robot)

            val wait = robotCosts.foldLeft(0) { case (wait, (resource, amount)) =>
              val aux = (amount - inventory.get(resource)) / robots.count(resource).toDouble
              Math.max(wait, aux.ceil.toInt)
            }

            val remaining = remainingMins - wait - 1

            Option.when(remaining > 0) {
              val mined = this.robots.mine
                .map { case (resource, amount) =>
                  resource -> amount * (wait + 1)
                }

              val inventory = this.inventory
                .increment(mined)
                .decrement(robotCosts)
                .map {
                  case (Resource.Geode, amount) =>
                    amount
                  case (resource, amount) =>
                    Math.min(amount, blueprint.highestCost(resource) * remainingMins)
                }

              val robots = this.robots.add(robot)

              State(remaining.minutes, inventory, robots)
            }
          }

    }

    private object State {

      def initial(remaining: Duration): State =
        State(remaining, Inventory.empty, RobotCrew.of(Resource.Ore -> 1))

    }

  }

  def run(lines: LazyList[String]): Result = {
    val factories = lines.map(Blueprint.parse).map(Factory.apply)

    val part1 = {
      val duration = 24.minutes
      factories.map(f => f.run(duration) * f.blueprint.id).sum
    }

    val part2 = {
      val duration = 32.minutes
      factories.take(3).map(_.run(duration)).product
    }

    Result(part1.toString, part2.toString)
  }

}
