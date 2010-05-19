/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer.optimize

import org.objectweb.asm._


/**
 * @author Aleksey Nikiforov (lex)
 */
/* private[optimize] */ class TemplateGen {
  def genByteCode(
    templateClassName: String, templateString: String, replaceString: String
  ) :Array[Byte] = {
    val reader = new ClassReader(templateClassName)
    val writer = new ClassWriter(0)

    reader.accept(
      new TemplateClassVisitor(
        templateString,
        replaceString,
        writer
      ),
      ClassReader.SKIP_DEBUG
    )
    writer.toByteArray()
  }
}

/* private[optimize] */ class TemplateGenException(msg: String) extends Exception(msg)

/* private[optimize] */ trait TemplateWorker {
  def template: String
  def replaceValue: String

  protected def replace(s: String) = {
    if (s != null) s.replace(template, replaceValue)
    else null
  }
  protected def arraySig(template: String) = {
    if (template.endsWith("Byte")) {
      "[B"
    }
    else if (template.endsWith("SShort")) {
      "[S"
    }
    else if (template.endsWith("UShort")) {
      "[C"
    }
    else if (template.endsWith("Int")) {
      "[I"
    }
    else if (template.endsWith("HalfFloat")) {
      "[S"
    }
    else if (template.endsWith("RawFloat")) {
      "[F"
    }
    else if (template.endsWith("RawDouble")) {
      "[D"
    }
    else {
      throw new TemplateGenException("Unable to parse template string.")
    }
  }
  protected def brackets(s: String) = "(" + s + ")"
  protected def fixMkArrayDesc(name: String, desc: String) = {
    if (name == "mkDataArray") {
      val templateSig = brackets(arraySig(template))
      if (desc.take(4) == templateSig) {
        val replaceSig = brackets(arraySig(replaceValue))
        replaceSig + desc.drop(4)
      }
      else desc
    }
    else desc
  }
}

/* private[optimize] */ class TemplateClassVisitor(
  val template: String, val replaceValue: String, val cv: ClassVisitor
) extends ClassVisitor with TemplateWorker {

  def visit(
    version: Int,
    access: Int,
    name: String,
    signature: String,
    superName: String,
    interfaces: Array[String]
  ) {
    val rinterfaces = (
      for (i <- interfaces) yield {
        replace(i)
      }
    ).toArray
    cv.visit(
      version, access, replace(name),
      replace(signature), superName, rinterfaces
    )
  }

  def visitSource(source: String, debug: String) { /* do nothing */ }

  def visitOuterClass(owner: String, name: String, desc: String) {
    throw new TemplateGenException(
      "Unable to parse a template class belonging to an outer class."
    )
  }

  def visitAnnotation(desc: String, visible: Boolean) :AnnotationVisitor = {
    if (!desc.contains("ScalaSignature")) cv.visitAnnotation(desc, visible)
    else null
  }

  def visitAttribute(attr: Attribute) {
    cv.visitAttribute(attr)
  }

  def visitInnerClass(
    name: String,
    outerName: String,
    innerName: String,
    access: Int
  ) {
    throw new TemplateGenException(
      "Unable to parse a template class with an inner class."
    )
  }

  def visitField(
    access: Int,
    name: String,
    desc: String,
    signature: String,
    value: Object
  ) :FieldVisitor = {
    cv.visitField(
      access, replace(name), replace(desc), replace(signature), value
    )
  }

  def visitMethod(
    access: Int,
    name: String,
    desc: String,
    signature: String,
    exceptions: Array[String]
  ) :MethodVisitor = {
    new TemplateMethodVisitor(
      template, replaceValue,
      cv.visitMethod(
        access, replace(name), replace(fixMkArrayDesc(name, desc)),
        replace(signature), exceptions
      )
    )
  }

  def visitEnd() { cv.visitEnd() }
}

/* private[optimize] */ class TemplateMethodVisitor(
  val template: String, val replaceValue: String, mv: MethodVisitor
) extends MethodAdapter(mv) with TemplateWorker {

  override def visitTypeInsn(opcode: Int, ttype: String) {
    val rtype =
      if (ttype == arraySig(template)) arraySig(replaceValue)
      else replace(ttype)
    mv.visitTypeInsn(opcode, rtype)
  }

  override def visitFieldInsn(
    opcode: Int, owner: String, name: String, desc: String
  ) {
    mv.visitFieldInsn(opcode, replace(owner), replace(name), replace(desc))
  }

  override def visitMethodInsn(
    opcode: Int, owner: String, name: String, desc: String
  ) {
    mv.visitMethodInsn(
      opcode, replace(owner), replace(name), replace(fixMkArrayDesc(name, desc))
    )
  }

  override def visitLocalVariable(
    name: String,
    desc: String,
    signature: String,
    start: Label,
    end: Label,
    index: Int
  ) {
    mv.visitLocalVariable(
      replace(name), replace(desc), replace(signature),
      start, end, index
    )
  }

  override def visitLineNumber(line: Int, start: Label) { /* do nothing */ }
}
