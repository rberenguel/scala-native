package scala.scalanative
package linker

import java.io.{File, PrintWriter}
import scala.collection.mutable
import nir._
import nir.serialization._
import nir.Shows._
import util.Scope

sealed trait Linker {

  /** Link the whole world under closed world assumption. */
  def link(entries: Seq[Global]): (Seq[Global], Seq[Attr.Link], Seq[Defn])
}

object Linker {

  /** Create a new linker given tools configuration. */
  def apply(paths: Seq[Path],
            injects: Seq[Injects],
            depends: Seq[Depends],
            reporter: Reporter): Linker =
    new Impl(paths, injects, depends, reporter)

  private final class Impl(paths: Seq[Path],
                           injects: Seq[Injects],
                           depends: Seq[Depends],
                           reporter: Reporter)
      extends Linker {
    import reporter._

    private def load(
        global: Global): Option[(Seq[Dep], Seq[Attr.Link], Defn)] =
      paths.collectFirst {
        case path if path.contains(global) =>
          path.load(global)
      }.flatten

    def link(entries: Seq[Global]): (Seq[Global], Seq[Attr.Link], Seq[Defn]) = {
      val resolved    = mutable.Set.empty[Global]
      val unresolved  = mutable.Set.empty[Global]
      val links       = mutable.Set.empty[Attr.Link]
      val defns       = mutable.UnrolledBuffer.empty[Defn]
      val direct      = mutable.Stack.empty[Global]
      var conditional = mutable.UnrolledBuffer.empty[Dep.Conditional]

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
                resolved += workitem
                defns += defn
                links ++= newlinks
                onResolved(workitem)

                deps.foreach {
                  case Dep.Direct(dep) =>
                    direct.push(dep)
                    onDirectDependency(workitem, dep)

                  case cond @ Dep.Conditional(dep, condition) =>
                    conditional += cond
                    onConditionalDependency(workitem, dep, condition)
                }
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

      val allEntries = entries ++ depends.flatMap(_.depends)

      onStart()

      allEntries.foreach { entry =>
        direct.push(entry)
        onEntry(entry)
      }

      while (direct.nonEmpty) {
        processDirect
        processConditional
      }

      onComplete()

      val injected    = injects.flatMap(_.injects)
      val resultDefns = (defns ++ injected).sortBy(_.name.toString).toSeq

      (unresolved.toSeq, links.toSeq, resultDefns)
    }
  }
}
