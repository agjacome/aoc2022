package dev.agjacome.aoc2022

object Day21 extends Day {

  sealed trait Job {

    import Job._

    def run: Long =
      this match {
        case Number(v) => v
        case Add(l, r) => l.run + r.run
        case Sub(l, r) => l.run - r.run
        case Mul(l, r) => l.run * r.run
        case Div(l, r) => l.run / r.run
        case _         => sys.error("unknown")
      }

    def solve: Long =
      this match {
        case Add(l, r) => l.solveFor(r)
        case Sub(l, r) => l.solveFor(r)
        case Mul(l, r) => l.solveFor(r)
        case Div(l, r) => l.solveFor(r)
        case _         => this.run
      }

    def solveFor(that: Job): Long = {
      this match {
        // unknown on lhs
        case Add(l, r) if l.isUnknown => l.solveFor(Sub(that, r))
        case Sub(l, r) if l.isUnknown => l.solveFor(Add(that, r))
        case Mul(l, r) if l.isUnknown => l.solveFor(Div(that, r))
        case Div(l, r) if l.isUnknown => l.solveFor(Mul(that, r))

        // unknown on rhs
        case Add(l, r) if r.isUnknown => r.solveFor(Sub(that, l))
        case Sub(l, r) if r.isUnknown => r.solveFor(Sub(l, that))
        case Mul(l, r) if r.isUnknown => r.solveFor(Div(that, l))
        case Div(l, r) if r.isUnknown => r.solveFor(Div(l, that))

        // otherwise
        case _ => if (that.isUnknown) that.solveFor(this) else that.run
      }
    }

    private def isUnknown: Boolean =
      this match {
        case Number(_) => false
        case Add(l, r) => l.isUnknown || r.isUnknown
        case Sub(l, r) => l.isUnknown || r.isUnknown
        case Mul(l, r) => l.isUnknown || r.isUnknown
        case Div(l, r) => l.isUnknown || r.isUnknown
        case Unknown   => true
      }

  }

  object Job {
    final case class Number(v: Long)     extends Job
    final case class Add(l: Job, r: Job) extends Job
    final case class Sub(l: Job, r: Job) extends Job
    final case class Mul(l: Job, r: Job) extends Job
    final case class Div(l: Job, r: Job) extends Job
    final case object Unknown            extends Job
  }

  final case class YellingMonkeys(monkeys: Map[String, String]) extends AnyVal {

    def monkeyYell(root: String): Job =
      tree(root, monkeyYell)

    def humanYell(human: String, root: String): Job =
      if (root == "humn")
        Job.Unknown
      else
        tree(root, humanYell(human, _))

    private def tree(name: String, build: String => Job): Job =
      monkeys(name) match {
        case s"$l + $r" => Job.Add(build(l), build(r))
        case s"$l - $r" => Job.Sub(build(l), build(r))
        case s"$l * $r" => Job.Mul(build(l), build(r))
        case s"$l / $r" => Job.Div(build(l), build(r))
        case otherwise  => Job.Number(otherwise.toLong)
      }

  }

  object YellingMonkeys {

    def parse(lines: LazyList[String]): YellingMonkeys = {
      val monkeys = lines.map { case s"$name: $expr" => name -> expr }.to(Map)
      YellingMonkeys(monkeys)
    }

  }

  def run(lines: LazyList[String]): Result = {
    val monkeys = YellingMonkeys.parse(lines)

    val part1 = monkeys.monkeyYell("root").run
    val part2 = monkeys.humanYell("humn", "root").solve

    Result(part1.toString, part2.toString)
  }

}
