package scala.scalanative
package linker
package inject

import nir._

/** Generate runtime type information. */
object RuntimeTypeInformation extends Inject with Depend {
  private def typeDefn(node: World.Scope) = node match {
    case cls: World.Class =>
      Defn.Const(Attrs.None, cls.typeName, cls.typeStruct, cls.typeValue)

    case node @ (_: World.Trait | _: World.Struct) =>
      val typeId  = Val.I32(node.id)
      val typeStr = Val.String(node.name.id)
      val typeVal = Val.Struct(Rt.Type.name, Seq(typeId, typeStr))

      Defn.Const(Attrs.None, node.typeName, Rt.Type, typeVal)

    case _ =>
      util.unreachable
  }

  override def depend =
    Seq(Rt.Type.name)

  override def inject(top: World.Top): Unit = {
    top.classes.foreach(cls => top.enter(typeDefn(cls)))
    top.traits.foreach(cls => top.enter(typeDefn(cls)))
    top.structs.foreach(cls => top.enter(typeDefn(cls)))
  }
}
