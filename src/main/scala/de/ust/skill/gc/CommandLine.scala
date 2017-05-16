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
    progress : Boolean = false,
    statistics : Boolean = false);

  val argumentParser = new scopt.OptionParser[GCConfig]("skillGC") {

    opt[Unit]('p', "progress").unbounded().action((x, c) ⇒
      c.copy(progress = true)).text("print progress while writing output file")

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

    val out =
      if (null == opts.output) System.out
      else new PrintStream(opts.output)

    MakeHTML(sf, opts.output, opts.progress, opts.statistics, out)

    if (opts.statistics) {
      println(s" finished in ${(System.nanoTime() - begin) * 1e-9} sec")
    }

    if (null != opts.output) {
      println("-done-")
    } else {
      out.close()
    }
  }
}