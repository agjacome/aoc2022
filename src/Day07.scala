package dev.agjacome.aoc2022

final case class Path(components: Vector[String]) extends AnyVal {

  def /(sub: String): Path =
    this.copy(components :+ sub)

  def parent: Path =
    this.copy(components.dropRight(1))

  def startsWith(prefix: Path): Boolean =
    components.startsWith(prefix.components)

}

object Path {

  val root = Path(Vector("/"))

}

final case class FileSystem(sizes: Map[Path, Int]) extends AnyVal {

  def +(kv: (Path, Int)): FileSystem = {
    val (path, size) = kv

    val updated = sizes
      .updatedWith(path)(_.orElse(Some(0)))
      .map { case (p, s) => (p -> (if (path.startsWith(p)) s + size else s)) }

    this.copy(updated)
  }

  def filter(f: Path => Boolean): FileSystem =
    this.copy(sizes.view.filterKeys(f).toMap)

}

object FileSystem {

  val empty = FileSystem(Map.empty)

  def fromCliLog(log: Iterator[String]): FileSystem = {
    val cd   = """^\$ cd (.+)$""".r
    val dir  = """^dir (.+)$""".r
    val file = """^(\d+) .+$""".r

    @scala.annotation.tailrec
    def loop(fs: FileSystem, currentPath: Path): FileSystem =
      log.nextOption() match {
        case None             => fs
        case Some(cd("/" ))   => loop(fs, Path.root)
        case Some(cd(".."))   => loop(fs, currentPath.parent)
        case Some(cd(dir ))   => loop(fs, currentPath / dir)
        case Some(dir(dir))   => loop(fs + (currentPath / dir -> 0), currentPath)
        case Some(file(size)) => loop(fs + (currentPath -> size.toInt), currentPath)
        case _                => loop(fs, currentPath)
      }

    loop(FileSystem.empty, Path.root)
  }

}


object Day07 extends Day {

  def run(lines: Iterator[String]): Result = {
    val fs    = FileSystem.fromCliLog(lines)
    val sizes = fs.sizes

    val part1 = {
      val threshold = 100_000

      sizes.values.filter(_ <= threshold).sum
    }

    val part2 = {
      val total  = 70_000_000
      val needed = 30_000_000

      val free      = total - sizes(Path.root)
      val threshold = needed - free

      sizes.values.filter(_ >= threshold).min
    }

    Result(part1.toString, part2.toString)
  }

}
