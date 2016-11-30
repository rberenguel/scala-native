package scala.scalanative
package optimizer

import tools.Config
import linker.World.Top
import nir.{Global, Defn}

trait PassCompanion extends linker.Depends with linker.Injects {

  /** Instantiate the given pass. */
  def apply(config: Config, top: Top): Pass
}
