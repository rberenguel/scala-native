package scala.scalanative
package linker
package inject

import nir._

/** Injects trait disptatch and instance tables. */
object TraitTables extends Inject {
  override def inject(top: World.Top): Unit = {
    //top.enter(top.dispatchDefn)
    //top.enter(top.instanceDefn)
  }
}
