/*
 * Simplex3dConsole
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dConsole.
 *
 * Simplex3dConsole is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dConsole is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.console

import java.awt.event.ActionEvent
import java.io.FileInputStream
import javax.swing.AbstractAction
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JTextArea


/**
 * @author Aleksey Nikiforov (lex)
 */
object Examples {

  def populateMenus(txt: JTextArea, scalaMenu: JMenu, simplex3dMenu: JMenu) {
    val is = getClass.getClassLoader.getResourceAsStream("simplex3d/console/examples.index")
    val index = scala.io.Source.fromInputStream(is).getLines().toList
    is.close()

    val root = new Node(null, true)
    root.addChild(new Node("scala", true))
    root.addChild(new Node("simplex3d", true))

    for (entry <- index) { root.addPath(entry) }
    root.sort()

    def mkMenus(node: Node, menu: JMenu) {
      for (child <- node.children) {
        if (child.isDir) {
          val childMenu = new JMenu(mkName(child.name))
          menu.add(childMenu)
          mkMenus(child, childMenu)
        }
        else {
          val item = new JMenuItem()
          menu.add(item)
          item.setAction(new SelectExampleAction(mkName(child.name), child.getResourcePath, txt))
        }
      }
    }

    def mkName(file: String) = {
      val idx = file.lastIndexOf(".")
      val name = if (idx > 0) file.take(idx) else file
      name.take(1).toUpperCase + name.drop(1)
    }

    mkMenus(root.findChild("scala").get, scalaMenu)
    mkMenus(root.findChild("simplex3d").get, simplex3dMenu)
  }

  def getExample(path: String) :String = {
    val fullPath = "simplex3d/console/example/" + path
    var is = getClass.getClassLoader.getResourceAsStream(fullPath)
    val name = path.take(path.lastIndexOf(".")).drop(path.lastIndexOf("/") + 1)

    if (is != null) {
      val code = scala.io.Source.fromInputStream(is).mkString
      is.close()
      extractInnerCode(name, code)
    }
    else {
      null
    }
  }

  private def extractInnerCode(name: String, code: String) :String = {
    val extIdx = code.indexOf("extends")
    val next0 = extIdx + "extends".length
    if (extIdx > 0) {
      val appIdx = code.indexOf("Application", next0)
      val next1 = appIdx + "Application".length
      if (appIdx > 0 && code.substring(next0, appIdx).trim.isEmpty) {
        val cbIdx = code.indexOf("{", next1)
        if (cbIdx > 0 && code.substring(next1, cbIdx).trim.isEmpty) {
          val endIdx = code.lastIndexOf("}")
          if (endIdx > 0) return "//" + name + "\n\n" + cleanup(code.substring(cbIdx + 1, endIdx))
        }
      }
    }

    code
  }

  private def cleanup(code: String) :String = {
    val lines = code.split("\n")
    val res = lines.dropWhile(_.trim.isEmpty).reverse.dropWhile(_.trim.isEmpty).reverse
    unindent(res).mkString("\n")
  }

  private def unindent(codeLines: Seq[String]) :Seq[String] = {
    if (codeLines.isEmpty) return codeLines

    val lines = codeLines.map(_.replace("\t", "  "))
    val Spaces = """^(\s*).*""".r
    val Spaces(hs) = lines.head
    var min = scala.Int.MaxValue

    for (line <- lines) {
      val Spaces(s) = line
      if (s != null && !line.trim.isEmpty && s.length < min) min = s.length
    }

    lines.map(_.drop(min))
  }
}

class SelectExampleAction(name: String, val path: String, txt: JTextArea)
extends AbstractAction(name) {
  override def actionPerformed(e: ActionEvent) {
    val code = Examples.getExample(path)
    if (code != null) {
      txt.setText(code)
      txt.getCaret.setDot(0)
      txt.requestFocus()
    }
  }
}

object NodeOrdering extends Ordering[Node] {
  def compare(a: Node, b: Node) :Int = {
    if (!(a.isDir ^ b.isDir)) a.name.toLowerCase.compareTo(b.name.toLowerCase)
    else if (a.isDir) -1
    else 1
  }
}

class Node(val name: String, val isDir: Boolean) {
  private var _children = List[Node]()
  private var _parent: Node = _

  def children = _children

  def addChild(child: Node) {
    if (!isDir) throw new UnsupportedOperationException

    child._parent = this
    _children = child :: _children
  }

  def findChild(name: String) :Option[Node] = {
    for (child <- children) {
      if (child.name == name) return Some(child)
    }
    return None
  }

  def addPath(path: String) { addPath(path.split("/").toList) }

  def addPath(path: List[String]) {
    if (!path.isEmpty) {
      findChild(path.head) match {
        case Some(child) => child.addPath(path.tail)
        case None => {
          val child = new Node(path.head, !path.tail.isEmpty)
          addChild(child)
          child.addPath(path.tail)
        }
      }
    }
  }

  def sort() {
    _children = _children.sorted(NodeOrdering)
    for (child <- children) {
      if (child.isDir) child.sort()
    }
  }

  def getResourcePath() :String = {
    var res = List(name)

    var cur = _parent; while (cur != null) {
      if (cur.name != null) res = cur.name :: res
      cur = cur._parent
    }

    res.mkString("/")
  }

  override def toString() :String = name
}
