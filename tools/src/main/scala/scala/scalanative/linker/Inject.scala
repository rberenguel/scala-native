package scala.scalanative
package linker

trait Inject {

  /** A sequence of extra definitions that should additionally be
   *  injected into final assembly.
   */
  def inject(top: World.Top): Unit = ()
}
