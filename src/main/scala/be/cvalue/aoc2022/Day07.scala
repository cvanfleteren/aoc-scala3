import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day07 extends App {

  val testInput =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin

  val input = Files.readString(Path.of("./src/main/resources/2022/day07.txt"))
  val input2 = """$ cd /
                |$ ls
                |149291 cgc.vzv
                |dir cmcrzdt
                |dir hwdvrrp
                |26925 hwqvsl
                |dir lsmv
                |dir ngfllcq
                |dir ngnzzmpc
                |dir pwhjps
                |dir rgwnzttf
                |260556 tcglclw.hsn
                |dir trvznjhb
                |dir wgcqrc
                |68873 whpnhm
                |$ cd cmcrzdt""".stripMargin

  trait Op

  case object Root extends Op

  case class cd(to: String) extends Op

  case class DirEntry(name: String) extends Op

  case class FileEntry(name: String, size: Int) extends Op

  case object `cd..` extends Op

  def parse(in: String): List[Op] = {

    in.split("\n").tail.filterNot(_.startsWith("$ ls")).map { line =>
      if (line.startsWith("$ cd ..")) {
        `cd..`
      } else if (line.startsWith("$ cd ")) {
        cd(line.substring(5))
      } else if (line.startsWith("dir")) {
        DirEntry(line.substring(4))
      } else {
        val parts = line.split(' ')
        FileEntry(parts(1), parts(0).toInt)
      }
    }.toList
  }

  def populate(ops: List[Op]) : Dir = {
    val populated = ops.foldLeft(Dir("/")) { case (dir, entry) =>
      entry match {
        case d: DirEntry => dir.add(d)
        case f: FileEntry => dir.add(f)
        case cd(to) => dir.dirs.find(_.name == to).get
        case `cd..` => dir.parent.get
      }
    }
    populated.root
  }

  def part1(in: String): String = {
    val dir = populate(parse(in))
    dir.root.allDirs.filter(_.size < 100000).map(_.size).sum.toString
  }

  def part2(in: String): String = {
    val root = populate(parse(in)).root

    val totalSpace = 70_000_000
    val spaceNeeded = 30_000_000
    val spaceUsed = root.size

    val needToDelete = spaceUsed - spaceNeeded



      //.find(_.size > needToDelete).get.size.toString
      ???
  }

  case class Dir(name: String, files: ListBuffer[(String, Int)] = ListBuffer.empty, dirs: ListBuffer[Dir] = ListBuffer.empty, parent:Option[Dir] = None) {

    def size:Int = files.map(_._2).sum + dirs.map(_.size).sum

    def add(dir:DirEntry) = copy(dirs = dirs.addOne(Dir(name=dir.name,parent=Some(this))))

    def add(file: FileEntry) = copy(files = files.addOne((file.name, file.size)))

    final def allDirs: List[Dir] = {
      dirs.toList ::: dirs.flatMap(_.allDirs).toList
    }

    @tailrec
    final def root: Dir = {
      parent match {
        case None => this
        case Some(p) => p.root
      }
    }

    override def toString: String = {
      s"""
         |- $name (dir)
         |${files.map(f => s"${f._1} (file, size=${f._2})").mkString("\n")}
         |${dirs.map(d => s"${d.name} (dir)").mkString("\n")}
         |""".stripMargin
    }

  }

  println(part1(input))
  println(part2(input))
}