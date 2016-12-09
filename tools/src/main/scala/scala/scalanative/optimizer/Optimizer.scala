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

    val passes = driver.passes.map(_.apply(config, world)).zipWithIndex

    def transform(node: World.Node, passes: Seq[(Pass, Int)]): Unit =
      passes match {
        case Seq() =>
          ()

        case (pass.EmptyPass, _) +: rest =>
          transform(node, rest)

        case (pass, id) +: rest =>
          pass(node)
          onPass(node, pass)
          transform(node, rest)
      }

    world.methods.foreach { method =>
      if (method.insts.nonEmpty) {
        onStart(method)
        transform(method, passes)
        onComplete(method)
      }
    }

    world.fields.foreach { field =>
      onStart(field)
      transform(field, passes)
      onComplete(field)
    }
  }
}
