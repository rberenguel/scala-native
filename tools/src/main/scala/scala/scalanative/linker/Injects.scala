package scala.scalanative
package linker

trait Injects {

  /** A sequence of extra definitions that should additionally be
   *  injected into final assembly.
   */
  def injects: Seq[nir.Defn] = Seq()
}
