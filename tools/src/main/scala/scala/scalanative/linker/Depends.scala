package scala.scalanative
package linker

trait Depends {

  /** A sequence of extra globals that should be loaded
   *  during linking of the assembly.
   */
  def depends: Seq[nir.Global] = Seq()
}
