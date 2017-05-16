package de.ust.skill.gc

import java.io.File

import de.ust.skill.common.scala.api.Access
import de.ust.skill.common.scala.api.FieldDeclaration
import de.ust.skill.common.scala.api.SkillObject
import de.ust.skill.common.scala.api.id
import de.ust.skill.gc.api.SkillFile
import de.ust.skill.common.scala.internal.fieldTypes.StringType
import java.io.PrintStream
import de.ust.skill.common.scala.internal.fieldTypes.SingleBaseTypeContainer
import de.ust.skill.common.scala.api.FieldType

/**
 * state of a garbage collection run
 *
 * @note type anchors have a prefix t
 * @note object anchors have a prefix o
 */
final object MakeHTML {

  def apply(sf : SkillFile, output : File, printProgress : Boolean, printStatistics : Boolean, out : PrintStream) {
    val types = sf.map(t ⇒ (t.name, t)).toMap : Map[String, Access[_]]

    out.println(s"""<!DOCTYPE html>
<html>
<head>
<title>sf2html: ${sf.path}</title>
</head>
<body>
<h1>Types</h1>""")

    for (t ← sf) {
      out.println(s"""${
        if (t.superName.isEmpty) "<p>"
        else""
      }
<a name=t${t.name}>${t.name}</a> ${t.superName.map(n ⇒ s"""&lt: <a href="#t$n">$n</a>""").getOrElse("")} : ${
        val s = t.size
        if (0 == s) "0"
        else s"""$s ${ref(t.head, "First")} ${ref(t.last, "Last")}"""
      }<br>
    """)
    }

    out.println("""
<h1>Objects</h1>
""")

    for (
      t ← sf if t.superName.isEmpty;
      i ← t;
      n = i.getTypeName
    ) out.println(s"""<h3><a name=o$n${id(i)}><a href="#t$n">$n</a>#${id(i)}</a></h3>${
      (for (f ← types(n).allFields)
        yield s"""
      ${f.t} ${f.name} = ${value(i, f)}"""
      ).mkString("<p>", "<br>", "<hr>")
    }""")

    out.println("""
</body>
</html>""")
  }

  private def ref(target : Access[_], text : String) : String = s"""<a href="#t${target.name}">$text</a>"""
  private def ref(target : SkillObject, text : String) : String = s"""<a href="#o${target.getTypeName}${id(target)}">$text</a>"""

  private def value(x : SkillObject, f : FieldDeclaration[_]) : String = vToString(f.getR(x), f.t)

  private def vToString(v : Any, t : FieldType[_]) : String = {
    if (null == v)
      "(null)"
    else
      t match {
        case t : SingleBaseTypeContainer[_, _] ⇒
          v.asInstanceOf[Iterable[_]].map(vToString(_, t.groundType)).mkString(t.toString() + "(", ", ", ")")

        case _ ⇒ v match {
          case v : String      ⇒ '"' + v + '"'
          case v : SkillObject ⇒ ref(v, s"${v.getTypeName}#${id(v)}")
          case _               ⇒ v.toString()
        }
      }
  }
}