package scala.scalanative
package optimizer

import nir._
import linker.World

/** Optimizer reporters can override one of the corresponding methods to
 *  get notified whenever one of the optimization events happens.
 */
object Optimizer {

  /** Run all of the passes on given assembly. */
  def apply(config: tools.Config,
            driver: Driver,
            world: World.Top,
            reporter: Reporter): Unit = {
    import reporter._

    val passes = driver.passes.map(_.apply(config, world))

    def transform(node: World.Node) = {
      onStart(node)
      passes.foreach { pass =>
        pass(node)
        onPass(node, pass)
      }
      onComplete(node)
    }

    world.methods.foreach(transform)
    world.fields.foreach(transform)
  }
}
