package de.ust.skill.gc

import java.io.File

import scala.collection.mutable.ArrayBuffer
import de.ust.skill.gc.api.SkillFile
import de.ust.skill.common.scala.api.Read
import de.ust.skill.common.scala.api.ReadOnly
import de.ust.skill.common.scala.api.Write
import scala.collection.mutable.HashSet
import java.io.PrintStream

final object CommandLine {
  case class GCConfig(
    target : File = null,
    output : File = null,
    directoryMode : Boolean = false,
    progress : Boolean = false,
    statistics : Boolean = false);

  val argumentParser = new scopt.OptionParser[GCConfig]("skillGC") {

    opt[Unit]('p', "progress").unbounded().action((x, c) ⇒
      c.copy(progress = true)).text("print progress while writing output file")

    opt[Unit]('d', "directory").action { (x, c) ⇒
      c.copy(directoryMode = true)
    }.text("create a directory instead of a single file. Recommended for files larger than 10 MB.")

    opt[File]('o', "output").action { (x, c) ⇒
      c.copy(output = x)
    }.text("add a type as gc root")

    opt[Unit]('s', "statistics").optional().action((x, c) ⇒
      c.copy(statistics = true)).text("print garbage statistics")

    help("help").text("prints this usage text")

    arg[File]("<file.sf>").action { (x, c) ⇒
      c.copy(target = x)
    }.text("target file")
  }

  def main(args : Array[String]) : Unit = {
    argumentParser.parse(args, GCConfig()).foreach(process)
  }

  private def process(opts : GCConfig) {

    val sf = SkillFile.open(opts.target, Read, ReadOnly);

    val begin = System.nanoTime()

    if (opts.directoryMode) {
      val base =
        if (null == opts.output) new File(".")
        else opts.output

      MakeHTMLDir(sf, base, opts.progress, opts.statistics)

    } else {
      val out =
        if (null == opts.output) System.out
        else new PrintStream(opts.output)

      MakeHTML(sf, opts.output, opts.progress, opts.statistics, out)

      if (null != opts.output)
        out.close()
    }

    if (opts.statistics) {
      println(s" finished in ${(System.nanoTime() - begin) * 1e-9} sec")
    }

    if (null != opts.output) {
      println("-done-")
    }
  }
}