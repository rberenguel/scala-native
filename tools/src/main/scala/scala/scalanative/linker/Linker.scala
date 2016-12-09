package scala.scalanative
package linker

import java.io.{File, PrintWriter}
import scala.collection.mutable
import nir._
import nir.serialization._
import nir.Shows._
import util.Scope
import World._

sealed trait Linker {

  /** Link the whole world under closed world assumption. */
  def link(entries: Seq[Global]): Top
}

object Linker {

  /** Sequence of default linker injections. */
  def injects: Seq[Inject] =
    Seq(inject.ClassLayout,
        inject.MainMethod,
        inject.ModuleAccessor,
        inject.RuntimeTypeInformation)

  /** Create a new linker given tools configuration. */
  def apply(config: tools.Config,
            injects: Seq[Inject],
            depends: Seq[Depend],
            reporter: Reporter): Linker =
    new Impl(config, injects, depends, reporter)

  private final class Impl(config: tools.Config,
                           injects: Seq[Inject],
                           depends: Seq[Depend],
                           reporter: Reporter)
      extends Linker {
    import reporter._

    private def load(
        global: Global): Option[(Seq[Dep], Seq[Attr.Link], Defn)] =
      config.paths.collectFirst {
        case path if path.contains(global) =>
          path.load(global)
      }.flatten

    def link(entries: Seq[Global]): Top = {
      val direct      = mutable.Stack.empty[Global]
      var conditional = mutable.UnrolledBuffer.empty[Dep.Conditional]
	  val resolved    = mutable.Map.empty[Global, Defn]
	  val unresolved  = mutable.Set.empty[Global]
      val links       = mutable.Set.empty[Attr.Link]

      def processDirect =
        while (direct.nonEmpty) {
          val workitem = direct.pop()

          if (!workitem.isIntrinsic && !resolved.contains(workitem) &&
              !unresolved.contains(workitem)) {

            load(workitem).fold[Unit] {
              unresolved += workitem

              onUnresolved(workitem)
            } {
              case (deps, newlinks, defn) =>
			    resolved(workitem) = defn
                links ++= newlinks

                deps.foreach {
                  case Dep.Direct(dep) =>
                    direct.push(dep)
                    onDirectDependency(workitem, dep)

                  case cond @ Dep.Conditional(dep, condition) =>
                    conditional += cond
                    onConditionalDependency(workitem, dep, condition)
                }

                onResolved(workitem)
            }
          }
        }

      def processConditional = {
        val rest = mutable.UnrolledBuffer.empty[Dep.Conditional]

        conditional.foreach {
          case Dep.Conditional(dep, cond)
              if resolved.contains(dep) || unresolved.contains(dep) =>
            ()

          case Dep.Conditional(dep, cond) if resolved.contains(cond) =>
            direct.push(dep)

          case dep =>
            rest += dep
        }

        conditional = rest
      }

      val allEntries = entries ++ depends.flatMap(_.depend)

      onStart()

      allEntries.foreach { entry =>
        direct.push(entry)
        onEntry(entry)
      }

      while (direct.nonEmpty) {
        processDirect
        processConditional
      }

      val top = new Top(config.main)
	  resolved.valuesIterator.foreach(top.enter(_))
      injects.foreach(_.inject(top))
      top.finish()

      onComplete()

      top
    }
  }
}
