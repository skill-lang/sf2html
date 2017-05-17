package de.ust.skill.gc

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.HashMap

import de.ust.skill.common.scala.api.Access
import de.ust.skill.common.scala.api.FieldDeclaration
import de.ust.skill.common.scala.api.FieldType
import de.ust.skill.common.scala.api.SkillObject
import de.ust.skill.common.scala.api.id
import de.ust.skill.common.scala.internal.StoragePool
import de.ust.skill.common.scala.internal.fieldTypes.SingleBaseTypeContainer
import de.ust.skill.gc.api.SkillFile

/**
 * create a set of directories that represent a file's content
 *
 * @note type anchors are named [[index.html]]#[[name]]
 * @note object anchors are named ./[[type]]/[[id]].html
 */
final object MakeHTMLDir {

  def apply(sf : SkillFile, base : File, printProgress : Boolean, printStatistics : Boolean) {
    val types = sf.map(t ⇒ (t.name, t)).toMap : Map[String, Access[_]]

    // create index
    new PrintWriter(new File(base, "index.html")) {
      write(s"""<!DOCTYPE html>
<html>
<head>
<title>sf2html: ${sf.path}</title>
</head>
<body>
<h1>Types</h1>
${
        (for (t ← sf)
          yield s"""${
          if (t.superName.isEmpty) "<p>"
          else""
        }
<a name=t${t.name}>${t.name}</a> ${t.superName.map(n ⇒ s"""&lt: <a href="#t$n">$n</a>""").getOrElse("")} : ${
          val s = t.size
          if (0 == s) "0"
          else s"""$s ${ref(t.head, "First")} ${ref(t.last, "Last")}"""
        }<br>
    """).mkString
      }
</body>
</html>"""); close
    }

    // create object dirs
    for (t ← sf if !t.asInstanceOf[StoragePool[_, _]].staticInstances.isEmpty)
      new File(base, t.name).mkdir()

    // create object files
    for (
      t ← sf if t.superName.isEmpty;
      i ← t;
      n = i.getTypeName
    ) {
      new PrintWriter(new File(new File(base, n), id(i) + ".html")) {
        val pool = types(n)
        write(s"""<!DOCTYPE html>
<html>
<head>
<title>sf2html: ${sf.path} -- $n ${id(i)}</title>
</head>
<body>
<h3><a name=o$n${id(i)}>${ref(pool, n, "../")}#${id(i)}</a></h3>${
          (for (f ← pool.allFields)
            yield s"""
      ${escape(f.t.toString)} ${f.name} = ${value(i, f)}"""
          ).mkString("<p>", "<br>", "")
        }
</body>
</html>"""); close
      }
    }
  }

  private def ref(target : Access[_], text : String, base : String = "") : String = s"""<a href="${base}index.html#t${target.name}">$text</a>"""
  private def ref(target : SkillObject, text : String, base : String) : String = s"""<a href="${base}${target.getTypeName}/${id(target)}.html">$text</a>"""
  private def ref(target : SkillObject, text : String) : String = s"""<a href="${target.getTypeName}/${id(target)}.html">$text</a>"""

  private def value(x : SkillObject, f : FieldDeclaration[_]) : String = vToString(f.getR(x), f.t)

  private def vToString(v : Any, t : FieldType[_]) : String = {
    if (null == v)
      "(null)"
    else
      t match {
        case t : SingleBaseTypeContainer[_, _] ⇒
          v.asInstanceOf[Iterable[_]].map(vToString(_, t.groundType)).mkString(escape(t.toString()) + "(", ", ", ")")

        case _ ⇒ v match {
          case v : String      ⇒ '"' + v + '"'
          case v : SkillObject ⇒ ref(v, s"${v.getTypeName}#${id(v)}", "../")
          case v : HashMap[_, _] ⇒ v.map { case (k, v) ⇒ s"${vToString(k, null)} -> ${vToString(v, null)}" }.mkString(
            if (null == t) "("
            else escape(t.toString()) + "(", ", ", ")")
          case _ ⇒ v.toString()
        }
      }
  }

  private def escape(s : String) : String = s.replace("<", "&lt").replace(">", "&gt")
}