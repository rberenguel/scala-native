package scala.scalanative
package linker

trait Depend {

  /** A sequence of extra globals that should be loaded
   *  during linking of the assembly.
   */
  def depend: Seq[nir.Global] = Seq()
}
