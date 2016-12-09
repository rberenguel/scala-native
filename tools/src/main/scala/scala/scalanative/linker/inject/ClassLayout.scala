package scala.scalanative
package linker
package inject

import nir._

/** Injects class layout structs. */
object ClassLayout extends Inject {
  override def inject(top: World.Top): Unit =
    top.classes.foreach { cls =>
      val layoutStructTy = cls.layoutStruct
      val layoutStructDefn =
        Defn.Struct(Attrs.None, layoutStructTy.name, layoutStructTy.tys)

      top.enter(layoutStructDefn)
    }
}
